import re
import os
import sys
import json
import time
import sqlite3
import random
import hashlib
import os.path
from functools import wraps
from operator import itemgetter
from datetime import date, datetime

from flask import Flask, render_template, request, redirect, abort, session
from flask_cors import CORS
from bs4.element import Tag
import requests
import aiml
import bs4

app = Flask(__name__)
app.config['SECRET_KEY'] = 'khfbyu4tgbukys'
CORS(app, supports_credential=True, resources={"/*", "*"})

time.clock = time.perf_counter
kernel = aiml.Kernel()
kernel.verbose(False)

for dir in os.listdir('aiml_sets'):
    for file in os.listdir(os.path.join('aiml_sets', dir)):
        kernel.learn(os.path.join('aiml_sets', dir, file))

with open('words.txt', encoding='utf-8') as f:
    words = list(map(str.strip, f))
    word_groups = [(c.upper(), [w for w in words if w.startswith(c)]) for c in 'abcdefghijklmnopqrstuvwxyz']

daily_updated = {
    'hitokoto': {},
    'the_wors': '',
    'news': {},
    'wallpapers': []
}
settings = {
    'state': True,
    'sound': True,
    'speed': 333
}
today = date.today()

def get_hitokoto():
    try:
        res = requests.get('https://v1.hitokoto.cn', timeout=5)
    except (requests.exceptions.ConnectTimeout, requests.exceptions.ReadTimeout):
        ...
    else:
        if res.status_code == 200:
            daily_updated['hitokoto'] = json.loads(res.text)
            return
    daily_updated['hitokoto'] = {
        'hitokoto': 'https://v1.hitokoto.cn坏掉了',
        'from_who': '系统',
        'from': '错误信息'
    }

def update(clear_muyu=True):
    global today
    today = date.today()
    
    text = requests.get('https://cctv.cn').content.decode()
    soup = bs4.BeautifulSoup(text, 'lxml')
    daily_updated['news'] = dict(
        focus=list(map(Tag.get_text, soup.find('div', {'class': 'list_lt9'}).find_all('li'))),
        **{categ: list(map(Tag.get_text, content.find_all('li')))
            for categ, content in zip(['social', 'global'], soup.find_all('div', {'class': 'col_w380_r'}))},
        channels=list(map(Tag.get_text, soup.find('div', {'class': 'col_w400'}).find_all('p', {'class': 'text'})))
    )
    
    res = requests.get('https://bing.com/HPImageArchive.aspx?format=js&idx=0&n=7')
    daily_updated['wallpapers'] = list(enumerate(json.loads(res.text)['images']))

    daily_updated['the_word'] = random.choice(words)
    get_hitokoto()

    
    with open('cache.json', 'w', encoding='utf-8') as file:
        file.write(json.dumps(daily_updated, ensure_ascii=False))
    
    if not clear_muyu:
        return
    connection = sqlite3.connect('qncblog.db')
    cursor = connection.cursor()
    cursor.execute('DELETE FROM muyu')
    connection.commit()
    connection.close()

def comp(tag, name, *cls):
    for i in cls:
        ret = tag.find(name, {'class': i})
        if ret is None:
            yield ''
        else:
            yield ret.string
            
def get_weather():
    res = requests.get('https://www.msn.cn/zh-cn/weather/forecast/')
    match = re.search(r'\<script id="redux-data" type="application/json"\>([\s\S]+?)\</script\>', res.text)
    return json.loads(match.group(1))

def query_word(word: str, verbose: bool):
    if verbose:
        connection = sqlite3.connect('qncblog.db')
        cursor = connection.cursor()
        cursor.execute('SELECT v FROM dict WHERE k = ?', (word,))
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
                ret['sens'].append((head, ['{} {}'.format(*comp(stc, 'div', 'bil_ex', 'val_ex'))
                                           for stc in body.find_all('div', {'class': 'li_ex'})]))
        cursor.execute('INSERT INTO dict(k, v) VALUES(?, ?)', (word, json.dumps(ret, ensure_ascii=False)))
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

def login_required(fn):
    @wraps(fn)
    def wrapper(*args, **kwargs):
        if 'id' not in session:
            return render_template('error.html', msg='未登录')
        return fn(*args, **kwargs, id=int(session.get('id')))
    return wrapper

def view(fn):
    @wraps(fn)
    def wrapper(*args, **kwargs):
        if request.remote_addr == '172.31.33.251':
            abort(500)
        if date.today() != today:
            update()
        return fn(*args, **kwargs)
    return wrapper

@app.route('/', methods=['GET', 'POST'])
@database_required
@view
def index(cursor: sqlite3.Cursor):
    if request.method == 'GET':
        args = dict.fromkeys(('times', 'issues', 'notices'))
        id = session.get('id')
        if id is not None:
            cursor.execute('SELECT COUNT(*) FROM issue WHERE author = ? AND time > ?', (id, today))
            args['times'] = cursor.fetchone()[0]
            cursor.execute('SELECT content FROM issue WHERE author = ? ORDER BY ID DESC', (id,))
            args['issues'] = map(itemgetter(0), cursor.fetchall())
            cursor.execute('SELECT html FROM notice WHERE target IS NULL OR target = ? ORDER BY ID DESC', (id,))
            args['notices'] = map(itemgetter(0), cursor.fetchall())
        return render_template(
            'index.html',
            id=id,
            name=session.get('name'),
            admin=session.get('id') == '20220905',
            **args
        )
    id, pwd = map(request.form.get, ('id', 'pwd'))
    if None in (id, pwd) or not id.isnumeric():
        return render_template('error.html', msg='表单校验错误')
    md5 = hashlib.md5()
    md5.update(pwd.encode())
    cursor.execute('SELECT name FROM user WHERE id = ? AND pwd = ?', (id, md5.hexdigest()))
    res = cursor.fetchone()
    if res is None:
        return render_template('error.html', msg='用户名或密码错误')
    session.update({
        'id': id,
        'name': res[0]
    })
    return redirect('/')

@app.route('/issue', methods=['POST'])
@login_required
@database_required
@view
def issues(id: int, cursor: sqlite3.Cursor):
    cursor.execute('SELECT COUNT(*) FROM issue WHERE author = ? AND time > ?', (
        id,
        today
    ))
    times = cursor.fetchone()[0]
    if times >= 5:
        return render_template('error.html', msg='反馈已达上限')
    content = request.form.get('content')
    if not content or content.isspace() or len(content) > 64:
        return render_template('error.html', msg='表单校验错误')
    cursor.execute('INSERT INTO issue(author, content, time) VALUES(?, ?, ?)', (
        id, 
        content,
        datetime.now()
    ))
    return render_template('success.html', msg='反馈成功')

@app.route('/mod-pwd', methods=['POST'])
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
        cursor.execute('SELECT * FROM user WHERE id = ? AND pwd = ?', (target, md5.hexdigest()))
        if cursor.fetchone() is None:
            return render_template('error.html', msg='旧密码错误')
    md5 = hashlib.md5()
    md5.update(new.encode())
    cursor.execute('UPDATE user SET pwd = ? WHERE id = ?', (md5.hexdigest(), target))
    return render_template('success.html', msg='修改密码成功, 现在的密码是' + new)

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
        return render_template(
            'query.html',
            maxlength=20,
            prompt='单词'
        )
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
    cursor.execute('SELECT data FROM sen WHERE word = ? AND offset = ?', (word, offset))
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
    cursor.execute('INSERT INTO sen(word, offset, data) VALUES(?, ?, ?)', (
        word, 
        offset, 
        json.dumps({'pages': pages, 'more': more})
    ))
    return render_template('more.html', more=more, pages=pages, word=word, offset=offset)

@app.route('/wenyan')
@database_required
@view
def wenyan(cursor: sqlite3.Cursor):
    word = request.args.get('word')
    if word is None:
        return render_template(
            'query.html',
            maxlength=10,
            prompt='字词'
        )
    word = word.strip().lower()
    if len(word) > 10:
        return render_template('error.html', msg='查询过长')
    if not word:
        return render_template('error.html', msg='查询过短')
    cursor.execute('SELECT v FROM wenyan WHERE k = ?', (word,))
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
        def maybe(cls: str, tagname: str):
            tag = soup.find('div', {'class': cls})
            if tag is None:
                return []
            return list(map(Tag.get_text, tag.find_all(tagname)))
        data = {
            'template_name_or_list': 'wenyan.html',
            'data': {
                '基本解释': maybe('jbjs', 'li'),
                '详细解释': maybe('xxjs', 'p'),
                '康熙字典': maybe('kxzd', 'p'),
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
    cursor.execute('INSERT INTO wenyan(k, v) VALUES(?, ?)', (word, json.dumps(data, ensure_ascii=False)))
    return render_template(
        **data
    )

@app.route('/hitokoto')
@view
def hitokoto():
    return render_template('hitokoto.html', **daily_updated['hitokoto'])

@app.route('/word')
@view
def word_sharing():
    return redirect('/query?verbose=yes&word=' + daily_updated['the_word'])

@app.route('/news')
@view
def news():
    return render_template('news.html', **daily_updated['news'])

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
            'WHERE month = ? AND day = ?)', 
            (today.month, today.day)
        )
        args['today'] = map(itemgetter(0), cursor.fetchall())
        cursor.execute(
            'SELECT u.name, b.day '
            'FROM birthday AS b '
            'JOIN user AS u '
            'ON b.id = u.id '
            'WHERE b.month = ? '
            'ORDER BY b.day',
            (today.month,)
        )
        args['thismonth'] = cursor.fetchall()
        args['month'] = today.month
        cursor.execute('SELECT COUNT(*) from birthday')
        args['count'] = cursor.fetchone()[0]
        args['login'] = 'id' in session
        args['profile'] = None
        if args['login']:
            cursor.execute('SELECT year, month, day FROM birthday WHERE id = ?', (session.get('id'),))
            res = cursor.fetchone()
            if res is not None:
                args['profile'] = (session.get('name'),) + res
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
    return render_template(
        'wallpaper.html', 
        imgs=daily_updated['wallpapers']
    )

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

@app.route('/muyu', methods=['GET', 'POST'])
@login_required
@database_required
@view
def muyu(id: int, cursor: sqlite3.Cursor):
    if not settings['state']:
        return render_template('error.html', msg='都什么年代了还在抽传统反馈')
    if request.method == 'GET':
        cursor.execute('SELECT count FROM muyu WHERE id = ?', (session.get('id'),))
        res = cursor.fetchone()
        return render_template(
            'muyu.html', 
            count=0 if res is None else res[0],
            sound=settings['sound'],
            autospeed=settings['speed']
        )
    count = request.form.get('count')
    if count is None or not count.isnumeric():
        return render_template('error.html', msg='表单校验错误')
    if count == '0':
        return redirect('/muyu')
    cursor.execute('SELECT count FROM muyu WHERE id = ?', (id,))
    res = cursor.fetchone()
    if res is None:
        cursor.execute('INSERT INTO muyu(id, count) VALUES(?, ?)', (id,count))
    else:
        cursor.execute('UPDATE muyu SET count = count + ? WHERE id = ?', (count, id))
    return redirect('/muyu')

@app.route('/muyu-ranking', methods=['GET', 'POST'])
@database_required
@view
def muyu_ranking(cursor: sqlite3.Cursor):
    if not settings['state']:
        return render_template('error.html', msg='都什么年代了还在抽传统反馈')
    id = session.get('id')
    if request.method == 'POST':
        anonymous = request.form.get('anonymous')
        if anonymous is None or not anonymous.isnumeric():
            return render_template('error.html', msg='表单校验错误')
        if id is None:
            return render_template('error.html', msg='未登录')
        cursor.execute('UPDATE muyu SET anonymous = ? WHERE id = ?', (anonymous, id))
        return redirect('/muyu')
    login = id is not None
    anonymous = False
    if login:
        cursor.execute('SELECT anonymous FROM muyu WHERE id = ?', (id,))
        res = cursor.fetchone()
        if res is None:
            login = False
        else:
            anonymous = res[0]
    cursor.execute(
        'SELECT u.name, m.id, m.count '
        'FROM muyu AS m '
        'JOIN user AS u '
        'ON u.id = m.id '
        'WHERE anonymous = 0 '
        'ORDER BY m.count DESC'
    )
    return render_template(
        'ranking.html', 
        data=enumerate(cursor.fetchall()),
        login=login,
        anonymous=anonymous
    )

@app.route('/muyu-enabled')
def muyu_enabled():
    return 'True' if settings['state'] else ''

@app.route('/edit-notices', methods=['GET', 'POST'])
@database_required
@view
def edit_notices(cursor: sqlite3.Cursor):
    if session.get('id') != '20220905':
        return render_template('error.html', msg='FUCK YOU')
    if request.method == 'GET':
        cursor.execute(
            'SELECT u.name, n.id, n.target, n.html '
            'FROM notice AS n '
            'LEFT JOIN user AS u '
            'ON n.target = u.id '
            'ORDER BY n.id DESC'
        )
        return render_template('editnotices.html', data=cursor.fetchall())
    if request.form.get('delete'):
        cursor.execute('DELETE FROM notice WHERE id = ?', (request.form.get('id'),))
    else:
        cursor.execute('UPDATE notice SET html = ? WHERE id = ?', (request.form.get('html'), request.form.get('id')))
    return redirect('/edit-notices')

@app.route('/ai', methods=['GET', 'POST'])
@login_required
@database_required
@view
def ai(id: int, cursor: sqlite3.Cursor):
    cursor.execute('SELECT COUNT(*) FROM ai WHERE user = ? AND time > ?', (id, today))
    count = cursor.fetchone()[0]
    if request.method == 'GET':
        cursor.execute('SELECT human, content FROM ai WHERE user = ? ORDER BY time', (id,))
        res = cursor.fetchall()
        return render_template('ai.html', session=res, count=count, name=session.get('name'))
    if count >= 20:
        return render_template('error.html', msg='使用次数过多')
    text = request.form.get('text')
    if not text or len(text) > 64:
        return render_template('error.html', msg='表单校验错误')
    now = datetime.now()
    cursor.execute('INSERT INTO ai(user, human, content, time) VALUES(?, ?, ?, ?)', (id, 1, text, now))
    response = kernel.respond(text, id)
    if not response:
        response = 'AIML没有响应'
    cursor.execute('INSERT INTO ai(user, human, content, time) VALUES(?, ?, ?, ?)', (id, 0, response, now))
    return redirect('/ai')

@app.route('/admin', methods=['GET', 'POST'])
@database_required
@view
def admin(cursor: sqlite3.Cursor):
    if session.get('id') != '20220905':
        return render_template('error.html', msg='FUCK YOU')
    if request.method == 'GET':
        cursor.execute(
            'SELECT u.name, i.author, i.content '
            'FROM issue AS i '
            'JOIN user AS u '
            'ON i.author = u.id '
            'ORDER BY i.id DESC'
        )
        return render_template('admin.html', issues=cursor.fetchall())
    match request.form:
        case {'task': 'set-muyu'}:
            save_settings(state=not settings['state'])
            return render_template('success.html', msg='木鱼设置为{}'.format(settings['state']))
        case {'task': 'set-sound'}:
            save_settings(sound=not settings['sound'])
            return render_template('success.html', msg='木鱼声音{}'.format(settings['sound']))
        case {'task': 'next-saying'}: 
            get_hitokoto()
            return redirect('/hitokoto')
        case {'task': 'edit-saying', **rest}:
            daily_updated['hitokoto'] = rest
            return redirect('/hitokoto')
        case {'task': 'send-notice', 'target': target, 'html': html}:
            cursor.execute(
                'INSERT INTO notice(target, html) VALUES(?, ?)',
                (target or None, html)
            )
            return render_template('success.html', msg='通知发送给了{}'.format(target))
        case {'task': 'mod-birthday', 'birthday': birthday, 'target': target}:
            birthday = date.fromisoformat(birthday)
            for attr in ('year', 'month', 'day'):
                cursor.execute(
                    'UPDATE birthday SET {} = ? WHERE id = ?'.format(attr),
                    (getattr(birthday, attr), target)
                )
            return render_template('success.html', msg='修改了' + target)
        case {'task': 'del-birthday', 'target': target}:
            cursor.execute('DELETE FROM birthday WHERE id = ?', (target,))
            return render_template('success.html', msg='删除了{}'.format(target))
        case {'task': 'query-id', 'id': id}:
            cursor.execute('SELECT name FROM user WHERE id = ?', (id,))
            res = cursor.fetchone()
            if res is None:
                return render_template('error.html', msg='未查询到' + id)
            return render_template('success.html', msg=res[0])
        case {'task': 'query-name', 'name': name}:
            cursor.execute('SELECT id, name FROM user WHERE name LIKE ?', ('%' + name + '%',))
            return render_template('success.html', msg=str(cursor.fetchall()))
        case {'task': 'login', 'id': id}:
            cursor.execute('SELECT name FROM user WHERE id = ?', (id,))
            session.update({
                'id': id,
                'name': cursor.fetchone()[0]
            })
            return redirect('/')
        case {'task': 'set-count', 'id': id, 'count': count}:
            if count == '0':
                cursor.execute('DELETE FROM muyu WHERE id = ?', (id,))
            else:
                cursor.execute('SELECT COUNT(*) FROM muyu WHERE id = ?', (id,))
                if cursor.fetchone()[0] > 0:
                    cursor.execute('UPDATE muyu SET count = ? WHERE id = ?', (count, id))
                else:
                    cursor.execute('INSERT INTO muyu(id, count) VALUES(?, ?)', (id, count))
            return render_template('success.html', msg='将{}的木鱼设置为{}'.format(id, count))
        case {'task': 'muyu-speed', 'speed': speed}:
            save_settings(speed=speed)
            return render_template('success.html', msg='将木鱼速度设置为{}'.format(speed))
        case {'task': 'clear-issues'}:
            cursor.execute('DELETE FROM issue')
            return render_template('success.html', msg='清除成功')
        case _:
            return render_template('error.html', msg='无效参数: ' + str(request.form))
        
def save_settings(**updates):
    settings.update(updates)
    with open('settings.json', 'w', encoding='utf-8') as file:
        file.write(json.dumps(settings))

if __name__ == '__main__':
    try:
        file = open('cache.json', encoding='utf-8')
    except OSError:
        update(False)
    else:
        daily_updated = json.load(file)
    try:
        file = open('settings.json', encoding='utf-8')
    except OSError:
        ...
    else:
        settings = json.load(file)
    if len(sys.argv) == 1:
        port = 1989
    else:
        port = int(sys.argv[1])
    app.run(
        port=port, 
        host='0.0.0.0'
    )