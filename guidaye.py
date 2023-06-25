import os
import argparse

import bs4
import requests
from bs4.element import NavigableString, Tag, ResultSet
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

def main(url: str, path: str) -> None:
    os.chdir(path)
    options = webdriver.EdgeOptions()
    options.add_argument('--headless')
    browser = webdriver.Edge(options=options)
    browser.get(url)
    n = 1
    count = 0
    index = open('index.html', 'w', encoding='utf-8')
    index.write('<p>书籍来源: <a href="#">b.guidaye.com</a></p>\n')
    while True:
        print('第', n, '页')
        soup = bs4.BeautifulSoup(browser.page_source, 'xml')
        links = soup.find('ul', id='js_cover_list').find_all('a')
        index.write(
            '\n'.join(
                '<a href="{}.html">{}</a><br>'.format(count + i, link.string)
                for i, link in enumerate(links)
        ))
        for link in links:
            soup = bs4.BeautifulSoup(requests.get('http://b.guidaye.com{}'.format(link['href'])).content.decode(), 'lxml')
            tags = []
            for tag in soup.find('div', class_='mb2'):
                if isinstance(tag, NavigableString):
                    continue
                if tag.name == 'img':
                    tags.append('<img src="https://b.guidaye.com{}">'.format(tag['src']))
                elif tag.string is None:
                    if tag.img is None:
                        tags.append('<p>{}</p>'.format(''.join(
                            str(i) for i in tag.children
                        )))
                    else:
                        tags.append('<img src="https://b.guidaye.com{}">'.format(tag.img['src']))
                else:
                    tags.append('<p>{}</p>'.format(tag.string))
            open('{}.html'.format(count), 'w', encoding='utf-8').write('\n'.join(tags))
            print('{}.html'.format(count), link.string)
            count += 1
        n += 1
        next = WebDriverWait(browser, 10).until(
            EC.presence_of_element_located((By.CLASS_NAME, 'next'))
        )
        if 'disable' in next.get_attribute('class'):
            break
        next.click()
    print('完成.')

def search(title: str) -> ResultSet:
    soup = bs4.BeautifulSoup(requests.post('https://b.guidaye.com/e/search/index.php', {
        'keyboard': title,
        'show': 'title',
        'tempid': 1
    }).content.decode(), 'lxml')
    return soup.find('ul', class_='search-novel-list').find_all('a')

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-title')
    parser.add_argument('-url')
    parser.add_argument('-path', default='.')
    args = parser.parse_args()
    if args.url:
        main(args.url, args.path)
    elif args.title:
        options = search(args.title)
        if not options:
            print('未找到')
        else:
            prompt = '选择书籍:\n{}\n'.format(
                '\n'.join(
                    '{} {}'.format(i, book.get_text())
                    for i, book in enumerate(options)
                )
            )
            url = 'https://b.guidaye.com{}'.format(options[int(input(prompt))]['href'])
            main(url, args.path)
    else:
        print('参数错误')