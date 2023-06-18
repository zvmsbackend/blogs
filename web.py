import sys
import json
import sqlite3
import random
import hashlib
from functools import wraps
from operator import itemgetter
from datetime import date, datetime

from flask import Flask, render_template, request, redirect, abort, session
from flask_cors import CORS
from bs4.element import Tag
import requests
import bs4

app = Flask(__name__)
app.config['SECRET_KEY'] = 'khfbyu4tgbukys'
CORS(app, supports_credential=True, resources={"/*", "*"})
app.test_request_context().push()

with open('words.txt', encoding='utf-8') as f:
    words = sorted(filter(bool, (w.strip().lower() for w in f.read().split('\n'))))
    word_groups = [(c.upper(), [w for w in words if w.startswith(c)]) for c in 'abcdefghijklmnopqrstuvwxyz']

today = date.today()
the_saying =  {}
the_word = ''
news = {}

def get_news():
    global news
    text = requests.get('https://cctv.cn').content.decode()
    soup = bs4.BeautifulSoup(text, 'lxml')
    news = dict(
        focus=list(map(Tag.get_text, soup.find('div', {'class': 'list_lt9'}).find_all('li'))),
        **{categ: list(map(Tag.get_text, content.find_all('li')))
            for categ, content in zip(['social', 'global'], soup.find_all('div', {'class': 'col_w380_r'}))},
        channels=list(map(Tag.get_text, soup.find('div', {'class': 'col_w400'}).find_all('p', {'class': 'text'})))
    )

def get_wallpapers():
    global wallpapers
    wallpapers = json.loads(requests.get('https://bing.com/HPImageArchive.aspx?format=js&mbl=1&idx=0&n=7&video=1').text)['images']

def get_the_word():
    global the_word
    the_word = random.choice(words)

def get_the_saying():
    global the_saying
    try:
        res = requests.get('https://v1.hitokoto.cn/')
        if res.status_code == 200:
            the_saying =  json.loads(res.text)
            return
    except requests.exceptions.ConnectTimeout:
        ...
    the_saying = {'hitokoto': 'https://v1.hitokoto.cn坏掉了w'}

def update():
    global today
    today = date.today()
    get_news()
    get_the_word()
    get_wallpapers()
    get_the_saying()

def comp(tag, name, *cls):
    for i in cls:
        ret = tag.find(name, {'class': i})
        if ret is None:
            yield ''
        else:
            yield ret.string
            
def get_weather():
    res = requests.get('https://www.msn.cn/zh-cn/weather/forecast/')
    soup = bs4.BeautifulSoup(res.text, 'lxml')
    return json.loads(soup.find('script', {'type': 'application/json'}).string)

def query_word(word: str, verbose: bool):
    if verbose:
        connection = sqlite3.connect('qncblog.db')
        cursor = connection.cursor()
        cursor.execute('SELECT v FROM dict WHERE k=?', (word,))
        res = cursor.fetchone()
        if res is not None:
            return json.loads(res[0])
        soup = bs4.BeautifulSoup(requests.get('https://cn.bing.com/dict?q=' + word).text, 'lxml')
        uls = list(soup.find_all('ul'))
        if len(uls) < 3:
            return {}
        pron = soup.find('div', {'class': 'hd_p1_1'})
        ret = {
            'explainations': list(map(Tag.get_text, uls[2].find_all('li'))),
            'pron': '' if pron is None else pron.get_text(),
            'sens': [],
            'more': []
        }
        main = soup.find('div', {'id': 'pos_0'})
        if main is not None:
            for head, body in zip(
                map(Tag.get_text, main.find_all('div', {'class': 'dis'})),
                main.find_all('div', {'class': 'li_exs'})
            ):
                ret['sens'].append((head, ['{} {}'.format(*comp(stc, 'div', 'bil_ex', 'val_ex')) for stc in body.find_all('div', {'class': 'li_ex'})]))
        cursor.execute('INSERT INTO dict VALUES(?, ?)', (word, json.dumps(ret, ensure_ascii=False)))
        connection.commit()
        connection.close()
        return ret
    soup = bs4.BeautifulSoup(requests.get('https://cn.bing.com/dict/SerpHoverTrans?q=' + word).text, 'lxml')
    return {'explainations': map(Tag.get_text, soup.find_all('li'))}

def database_required(db: str):
    if not isinstance(db, str):
        return database_required('qncblog.db')(db)
    def deco(fn):
        @wraps(fn)
        def wrapper(*args, **kwargs):
            connection = sqlite3.connect(db)
            cursor = connection.cursor()
            try:
                ret = fn(*args, **kwargs, cursor=cursor)
                connection.commit()
                return ret
            finally:
                connection.close()
            abort(500)
        return wrapper
    return deco

def view(fn):
    @wraps(fn)
    def wrapper(*args, **kwargs):
        if request.remote_addr == '172.31.33.251':
            abort(500)
        if date.today() != today:
            update()
        return fn(*args, **kwargs)
    return wrapper

@app.route('/')
@view
def index():
    return render_template(
        'index.html',
        name=session.get('name'),
        admin=session.get('id') == '20220905'
    )

@app.route('/login', methods=['GET', 'POST'])
@database_required
@view
def login(cursor: sqlite3.Cursor):
    if request.method == 'GET':
        return render_template('login.html')
    id, pwd = map(request.form.get, ('username', 'password'))
    if None in (id, pwd) or not id.isnumeric():
        return render_template('error.html', msg='表单校验错误')
    md5 = hashlib.md5()
    md5.update(pwd.encode())
    cursor.execute('SELECT name FROM user WHERE id=? AND pwd=?', (id, md5.hexdigest()))
    res = cursor.fetchone()
    if res is None:
        return render_template('error.html', msg='用户名或密码错误')
    session.update({
        'id': id,
        'name': res[0]
    })
    return redirect('/')

@app.route('/logout')
@view
def logout():
    session.pop('id')
    session.pop('name')
    return redirect('/')

@app.route('/query')
@view
def query():
    word = request.args.get('word')
    if word is None:
        return render_template('query.html')
    word = word.strip().lower()
    if len(word) > 20:
        return render_template('error.html', msg='查询过长')
    if not word:
        return render_template('error.html', msg='查询过短')
    args = query_word(word, request.args.get('verbose'))
    return render_template(
        'result.html',
        word=word,
        **args
    )

@app.route('/more')
@database_required
@view
def more(cursor: sqlite3.Cursor):
    word, offset = map(request.args.get, ('word', 'offset'))
    if None in (word, offset):
        return render_template('error.html', msg='缺乏参数')
    try:
        offset = int(offset)
    except (ValueError, TypeError):
        return render_template('error.html', msg='参数类型错误')
    cursor.execute('SELECT data FROM sen WHERE word=? AND offset=?', (word, offset))
    res = cursor.fetchone()
    if res is not None:
        return render_template('more.html', **json.loads(res[0]), word=word, offset=offset)
    res = requests.get('https://cn.bing.com/dict/service?q={}&offset={}&dtype=sen&&qs=n'.format(word, offset * 10 - 10))
    soup = bs4.BeautifulSoup(res.text, 'lxml')
    pages = soup.find('div', {'class': 'b_pag'})
    if pages is None:
        return render_template('error.html', msg='无相关结果')
    pages = list(map(Tag.get_text, soup.find('div', {'class': 'b_pag'}).find_all('a', {'class': 'b_primtxt'})))
    more = list(map(Tag.get_text, soup.find_all('div', {'class': 'se_li'})))
    cursor.execute('INSERT INTO sen VALUES(?, ?, ?)', (word, offset, json.dumps({'pages': pages, 'more': more})))
    return render_template('more.html', more=more, pages=pages, word=word, offset=offset)

@app.route('/wenyan')
@database_required
@view
def wenyan(cursor: sqlite3.Cursor):
    word = request.args.get('word')
    if word is None:
        return render_template('query.html')
    word = word.strip().lower()
    if len(word) > 10:
        return render_template('error.html', msg='查询过长')
    if not word:
        return render_template('error.html', msg='查询过短')
    cursor.execute('SELECT v FROM wenyan WHERE k=?', (word,))
    res = cursor.fetchone()
    if res is not None:
        return render_template(
            **json.loads(res[0])
        )
    res = requests.get('https://www.zdic.net/hans/' + word)
    if not res.text:
        return render_template('error.html', msg='查无此结果')
    soup = bs4.BeautifulSoup(res.text, 'lxml')
    if res.url.startswith('https://www.zdic.net/e/sci/index.php'):
        if soup.find('li') is None:
            return render_template('error.html', msg='未找到结果')
        items = [i.get_text().rstrip(i.find('span').string) for i in soup.find('div', {'class': 'sslist'}).find_all('a')]
        data = {
            'template_name_or_list': 'wenyan_search.html',
            'count': len(items),
            'items': items
        }
    elif len(word) == 1:
        swjz = soup.find('div', {'class': 'swjz'})
        data = {
            'template_name_or_list': 'wenyan.html',
            'data': {
                '基本解释': list(map(Tag.get_text, soup.find('div', {'class': 'jbjs'}).find_all('li'))),
                '详细解释': list(map(Tag.get_text, soup.find('div', {'class': 'xxjs'}).find_all('p'))),
                '康熙字典': list(map(Tag.get_text, soup.find('div', {'class': 'kxzd'}).find_all('p'))),
                '说文解字': [swjz.find('p').get_text() if swjz is not None else '']
            }
        }
    else:
        data = {
            'template_name_or_list': 'wenyan.html',
            'data': {}
        }
        for head, cls in (('词语解释', 'jbjs'), ('网络解释', 'wljs')):
            tag = soup.find('div', {'class': cls})
            data['data'][head] = [] if tag is None else list(map(Tag.get_text, tag.find_all('li')))
    cursor.execute('INSERT INTO wenyan VALUES(?, ?)', (word, json.dumps(data, ensure_ascii=False)))
    return render_template(
        **data
    )

@app.route('/saying')
@view
def everyday_saying():
    return render_template('saying.html', **the_saying)

@app.route('/word')
@view
def word_sharing():
    return redirect('/query?verbose=yes&word=' + the_word)

@app.route('/news')
@view
def news_zz():
    return render_template('news.html', **news)

@app.route('/birthday', methods=['GET', 'POST'])
@database_required
@view
def birthday(cursor: sqlite3.Cursor):
    if request.method == 'GET':
        args = {}
        cursor.execute(
            'SELECT name '
            'FROM user '
            'WHERE id IN '
            '(SELECT id '
            'FROM birthday '
            'WHERE month=? AND day=?)', 
            (today.month, today.day)
        )
        args['today'] = map(itemgetter(0), cursor.fetchall())
        cursor.execute(
            'SELECT u.name, b.day '
            'FROM birthday AS b '
            'JOIN user AS u '
            'ON b.id=u.id '
            'WHERE b.month=?',
            (today.month,)
        )
        args['thismonth'] = cursor.fetchall()
        args['month'] = today.month
        cursor.execute('SELECT COUNT(*) from birthday')
        args['count'] = cursor.fetchone()[0]
        args['login'] = 'id' in session
        args['profile'] = None
        if args['login']:
            cursor.execute('SELECT year, month, day FROM birthday WHERE id=?', (session.get('id'),))
            args['profile'] = (session.get('name'),) + cursor.fetchone()
        return render_template('birthday.html', **args)
    if 'id' not in session:
        return render_template('error.html', msg='未登录')
    try:
        birthday = date.fromisoformat(request.form.get('birthday'))
    except (ValueError, TypeError):
        return render_template('error.html', msg='错误的日期格式')
    try:
        cursor.execute('INSERT INTO birthday(id, year, month, day) VALUES(?, ?, ?, ?)', (
            session.get('id'), 
            birthday.year, 
            birthday.month, 
            birthday.day
        ))
    except sqlite3.IntegrityError:
        return render_template('error.html', msg='生日已注册')
    return redirect('/birthday')

@app.route('/wallpapers')
@view
def bing_wallpapers():
    return render_template('wallpaper.html', imgs=wallpapers)

@app.route('/weather')
@view
def msn_weather():
    weather = get_weather()
    return render_template(
        'weather.html',
        **weather['WeatherData']['_@STATE@_'],
        datetime=datetime,
        zip=zip,
        location=weather['WeatherPageMeta']['_@STATE@_']['location']['displayName']
    )

@app.route('/3500')
@view
def words_3500():
    q = request.args.get('q')
    if q is None:
        return render_template('3500.html', items=word_groups)
    if len(q) < 2:
        return render_template('error.html', msg='关键词过短')
    res = [w for w in words if q in w]
    if len(res) == 1:
        return redirect('/query?verbose=yes&word=' + res[0])
    return render_template('3500.html', items=[(f'共搜索到{len(res)}个单词', res)])

@app.route('/admin', methods=['GET', 'POST'])
@database_required
@view
def admin(cursor: sqlite3.Cursor):
    if session.get('id') != '20220905':
        return render_template('error.html', msg='FUCK YOU')
    if request.method == 'GET':
        cursor.execute(
            'SELECT u.name, i.content '
            'FROM issue AS i '
            'JOIN user AS u '
            'ON i.author=u.id '
            'ORDER BY i.id DESC'
        )
        return render_template('admin.html', issues=cursor.fetchall())
    match request.form.get('task'):
        case 'next-saying': 
            get_the_saying()
            return redirect('/saying')
        case 'edit-saying':
            global the_saying
            the_saying = {k: v for k, v in request.form.items() if k != 'task'}
            return redirect('/saying')
        case 'send-notice':
            cursor.execute(
                'INSERT INTO notice(target, html) VALUES(?, ?)',
                (request.form.get('target') or None, request.form.get('html'))
            )
            return redirect('/')
        case 'mod-birthday':
            birthday = date.fromisoformat(request.form.get('birthday'))
            for attr in ('year', 'month', 'day'):
                cursor.execute(
                    'UPDATE birthday SET {}=? WHERE id=?'.format(attr),
                    (getattr(birthday, attr), request.form.get('target'))
                )
            return redirect('/')
        
@app.route('/notices')
@database_required
@view
def notices(cursor: sqlite3.Cursor):
    cursor.execute('SELECT html FROM notice WHERE target IS NULL OR target=? ORDER BY id DESC', (session.get('id'),))
    return render_template('notices.html', notices=map(itemgetter(0), cursor.fetchall()))

@app.route('/issues', methods=['GET', 'POST'])
@database_required
@view
def issues(cursor: sqlite3.Cursor):
    if 'id' not in session:
        return render_template('error.html', msg='未登录')
    if request.method == 'GET':
        cursor.execute('SELECT content FROM issue WHERE author=?', (session.get('id'),))
        return render_template('issues.html', issues=map(itemgetter(0), cursor.fetchall()))
    content = request.form.get('content')
    if content is None or len(content) > 64:
        return render_template('error.html', msg='表单校验错误')
    cursor.execute('INSERT INTO issue(author, content) VALUES(?, ?)', (session.get('id'), content))
    return redirect('/')

@app.route('/mod-pwd', methods=['GET', 'POST'])
@database_required
@view
def mod_pwd(cursor: sqlite3.Cursor):
    if request.method == 'GET':
        return render_template('modpwd.html', target=session.get('id'))
    target, old, new = map(request.form.get, ('target', 'old', 'new'))
    if session.get('id') != '20220905' and None in (target, old, new):
        return render_template('error.html', msg='表单校验错误')
    if target != session.get('id') and session.get('id') != '20220905':
        return render_template('error.html', msg='FUCK YOU')
    if session.get('id') != '20220905':
        md5 = hashlib.md5()
        md5.update(old.encode())
        cursor.execute('SELECT * FROM user WHERE id=? AND pwd=?', (target, md5.hexdigest()))
        if cursor.fetchone() is None:
            return render_template('error.html', msg='旧密码错误')
    md5 = hashlib.md5()
    md5.update(new.encode())
    cursor.execute('UPDATE user SET pwd=? WHERE id=?', (md5.hexdigest(), target))
    return redirect('/')

if __name__ == '__main__':
    if len(sys.argv) == 1:
        port = 1989
    else:
        port = int(sys.argv[1])
    update()
    app.run(
        port=port, 
        host='0.0.0.0'
    )