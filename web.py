import re
import time
import sys
import json
import sqlite3
import random
import hashlib
import logging
import traceback
from threading import Lock
from functools import wraps
from operator import itemgetter
from datetime import date, datetime

from flask import Flask, render_template, request, redirect, abort, session
from tornado.httpserver import HTTPServer
from tornado.wsgi import WSGIContainer
from tornado.ioloop import IOLoop
from flask_cors import CORS
from bs4.element import Tag
import requests
import aiml
import bs4

app = Flask(__name__)
app.config['SECRET_KEY'] = 'khfbyu4tgbukys'
CORS(app, supports_credential=True, resources={"/*", "*"})

logger = logging.getLogger()
logging.basicConfig(level=logging.INFO)

time.clock = time.perf_counter
kernel = aiml.Kernel()
kernel.verbose(False)
kernel.loadBrain('aiml_sets/brain')

connection = sqlite3.connect('qncblog.db')
cursor = connection.cursor()
cursor.execute('SELECT user, session FROM ai')
for user, aisession in cursor.fetchall():
    kernel._sessions[user] = json.loads(aisession)
    kernel._sessions[user]['_inputStack'] = []
    for field, flag in (('_inputHistory', 1), ('_outputHistory', 0)):
        cursor.execute('SELECT content FROM aichat WHERE user = ? AND human = ?', (user, flag))
        kernel._sessions[user][field] = list(map(itemgetter(0), cursor.fetchall()))
connection.commit()
connection.close()

with open('words.txt', encoding='utf-8') as file:
    words = list(map(str.strip, file))

daily_updated = {
    'today': date.today(),
    'hitokoto': {},
    'the_wors': '',
    'news': {},
    'wallpapers': []
}
settings = {
    'muyu': True,
    'sound': True,
    'speed': 333,
    'offline': False
}

lock = Lock()

def save_updated():
    tmp = daily_updated.copy()
    tmp['today'] = tmp['today'].isoformat()
    with open('assets.json', 'w', encoding='utf-8') as file:
        json.dump(tmp, file, ensure_ascii=False)

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
    save_updated()

def update(clear_muyu=True):
    daily_updated['today'] = date.today()
    
    text = requests.get('https://cctv.cn').content.decode()
    soup = bs4.BeautifulSoup(text, 'lxml')
    daily_updated['news'] = dict(
        focus=list(map(Tag.get_text, soup.find('div', {'class': 'list_lt9'}).find_all('li'))),
        **{categ: list(map(Tag.get_text, content.find_all('li')))
            for categ, content in zip(['social', 'global'], soup.find_all('div', {'class': 'col_w380_r'}))},
        channels=list(map(Tag.get_text, soup.find('div', {'class': 'col_w400'}).find_all('p', {'class': 'text'})))
    )
    
    res = requests.get('https://bing.com/HPImageArchive.aspx?format=js&idx=0&n=7')
    daily_updated['wallpapers'] = []
    for i, img in enumerate(json.loads(res.text)['images']):
        daily_updated['wallpapers'].append((i, {
            'url': 'https://bing.com' + img['url'],
            'title': img['title'],
            'copyright': img['copyright']
        }))

    daily_updated['the_word'] = random.choice(words)
    get_hitokoto()

    save_updated()
    logger.info('Updated')
    
    if not clear_muyu:
        return
    connection = sqlite3.connect('qncblog.db')
    cursor = connection.cursor()
    cursor.execute('DELETE FROM muyu')
    connection.commit()
    connection.close()
    logger.info('Muyu cleared')

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
            with lock:
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
        if date.today() != daily_updated['today']:
            update()
        try:
            return fn(*args, **kwargs)
        except sqlite3.OperationalError:
            traceback.print_exc()
            return render_template('error.html', msg='可能数据库被锁了')
        except Exception as ex:
            traceback.print_exc()
            return render_template('error.html', msg='服务器遇到了{}'.format(ex.__class__.__qualname__))
    return wrapper

@app.route('/', methods=['GET', 'POST'])
@database_required
@view
def index(cursor: sqlite3.Cursor):
    if request.method == 'GET':
        args = dict.fromkeys(('times', 'issues', 'notices'))
        id = session.get('id')
        if id is not None:
            cursor.execute('SELECT COUNT(*) FROM issue WHERE author = ? AND time > ?', (id, daily_updated['today']))
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

@app.route('/favicon.ico')
def icon():
    return redirect('/static/favicon.ico')

@app.route('/issue', methods=['POST'])
@login_required
@database_required
@view
def issues(id: int, cursor: sqlite3.Cursor):
    cursor.execute('SELECT COUNT(*) FROM issue WHERE author = ? AND time > ?', (
        id,
        daily_updated['today']
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
        today = daily_updated['today']
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
    return render_template('3500.html')

@app.route('/muyu', methods=['GET', 'POST'])
@database_required
@view
def muyu(cursor: sqlite3.Cursor):
    if not settings['muyu']:
        return render_template('error.html', msg='都什么年代了还在抽传统反馈')
    id = session.get('id')
    if request.method == 'GET':
        cursor.execute('SELECT count FROM muyu WHERE id = ?', (id,))
        res = cursor.fetchone()
        return render_template(
            'muyu.html', 
            count=0 if res is None else res[0],
            sound=settings['sound'],
            autospeed=settings['speed'],
            offline=('false', 'true')[settings['offline']]
        )
    count = request.form.get('count')
    if count is None or not count.isnumeric():
        return render_template('error.html', msg='表单校验错误')
    if count == '0':
        return redirect('/muyu')
    cursor.execute('SELECT count FROM muyu WHERE id = ?', (id,))
    res = cursor.fetchone()
    if res is None:
        cursor.execute('INSERT INTO muyu(id, count, anonymous) VALUES(?, ?, 0)', (id, count))
    else:
        cursor.execute('UPDATE muyu SET count = count + ? WHERE id = ?', (count, id))
    return redirect('/muyu')

@app.route('/muyu-ranking', methods=['GET', 'POST'])
@database_required
@view
def muyu_ranking(cursor: sqlite3.Cursor):
    if not settings['muyu']:
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
    return ('false', 'true')[settings['muyu']]

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
    cursor.execute('SELECT COUNT(*) FROM aichat WHERE user = ? AND time > ?', (id, daily_updated['today']))
    count = cursor.fetchone()[0]
    if request.method == 'GET':
        cursor.execute('SELECT human, content FROM aichat WHERE user = ? ORDER BY time', (id,))
        res = cursor.fetchall()
        return render_template('ai.html', session=res, count=count, name=session.get('name'))
    if count >= 40:
        return render_template('error.html', msg='使用次数过多')
    text = request.form.get('text')
    if not text or len(text) > 256:
        return render_template('error.html', msg='表单校验错误')
    if 'sing' in text:
        return render_template('error.html', msg='你这小子, 想让AIML唱歌是罢')
    now = datetime.now()
    response = kernel.respond(text, id)
    if not response:
        return render_template('error.html', msg='AIML没有响应')
    cursor.execute('INSERT INTO aichat(user, human, content, time) VALUES(?, 1, ?, ?)', (id, text, now))
    cursor.execute('INSERT INTO aichat(user, human, content, time) VALUES(?, 0, ?, ?)', (id, response, now))
    aisession = json.dumps({k: v for k, v in kernel._sessions[id].items() if k[0] != '_'}, ensure_ascii=False)
    cursor.execute('SELECT COUNT(*) FROM ai WHERE user = ?', (id,))
    if cursor.fetchone()[0] == 0:
        cursor.execute('INSERT INTO ai(user, session) VALUES(?, ?)', (id, aisession))
    else:
        cursor.execute('UPDATE ai SET session = ? WHERE user = ?', (aisession, id))
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
        return render_template('admin.html', issues=cursor.fetchall(), settings=settings)
    match request.form:
        case {'task': 'set-muyu'}:
            save_settings(state=not settings['muyu'])
            return render_template('success.html', msg='木鱼设置为{}'.format(settings['muyu']))
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
            cursor.execute(
                'UPDATE birthday '
                'SET year = ?, month = ?, day = ? '
                'WHERE id = ?',
                (birthday.year, birthday.month, birthday.day, target)
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
        case {'task': 'set-offline'}:
            save_settings(offline=not settings['offline'])
            return render_template('success.html', msg='木鱼{}允许离线运行'.format('' if settings['offline'] else '不'))
        case {'task': 'clear-issues'}:
            cursor.execute('DELETE FROM issue')
            return render_template('success.html', msg='清除成功')
        case _:
            return render_template('error.html', msg='无效参数: ' + str(request.form))
        
def save_settings(**updates):
    settings.update(updates)
    with open('settings.json', 'w', encoding='utf-8') as file:
        json.dump(settings, file)

if __name__ == '__main__':
    try:
        file = open('assets.json', encoding='utf-8')
    except OSError:
        update(False)
    else:
        daily_updated = json.load(file)
        file.close()
    try:
        file = open('settings.json', encoding='utf-8')
    except OSError:
        ...
    else:
        settings = json.load(file)
        file.close()
    if len(sys.argv) == 1:
        port = 19198
    else:
        port = int(sys.argv[1])
    wsgi = WSGIContainer(app)
    server = HTTPServer(wsgi)
    server.listen(port)
    logger.info('Server started')
    IOLoop.instance().start()