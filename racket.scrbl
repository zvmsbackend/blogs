#lang scribble/manual

@(require "util.rkt")

@title{What is Racket}

@cursor["Cursor"]{
Racket, dear Racket, how we love thee
Your syntax so simple, yet so mighty
}

@italic{Racket}诞生于1994年, 比Java还"古老", 是1975年Scheme语言的一款方言. 而Scheme又源于1958年世界上第二古老的编程语言Lisp.
Racket继承了Scheme的简洁语法和函数式思想, 兼具Lisp"程序即数据"的核心原则, 加之丰富的库和相对完善的社区, Racket已成为一支重要的Lisp方言.

@section{安装与使用}

访问官网@hyperlink["#" "https://racket-lang.org"]下载安装包. 网站上还有包索引(@hyperlink["#" "https://pkgs.racket-lang.org"])和文档(@hyperlink["#" "https://docs.racket-lang.org"])

将Racket下载到本地后, 将其所在文件夹添加至PATH环境变量, 然后运行Racket即可体验.

@section{编辑器支持}

Racket自带了一款用Racket语言实现的IDE, DrRacket, 相当于Python的IDLE, 可以直接使用.

然而, DrRacket功能较少, 也不符合许多人的使用习惯. 幸好Racket社区提供了其他编辑器的支持. @hyperlink["#" "VSCode"]的Magic Racket插件就是一个不错的选择.

Magic Racket只自带了有限的功能, 如语法高亮, 批量注释等. 要想获取完全的功能, 需要下载Racket LSP(Language Server Protocol).

方法一:

打开终端, 输入

@codeblock{
$ raco pkg install racket-langserver
}

raco是Racket自带的命令行工具, 这里假定它已被添加到路径中

方法二:

输入

@codeblock{
$ git clone https://github.com/jeapostrophe/racket-langserver.git
}

接着
@codeblock{
$ cd racket-langserver
$ raco pkg install
}

如果提示需要依赖项, 可以用raco安装, 也可以先访问@hyperlink["#" "Racket Pakages Index"], 找到对应的包后克隆/下载到本地再安装