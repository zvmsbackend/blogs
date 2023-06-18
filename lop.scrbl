#lang scribble/manual

@(require "util.rkt")

@title{Language-Oriented Programming}

我们偶尔会听到某个"语言"是面向对象的, 抑或是过程式的. 那"面向语言"又是上面呢?

一般来说, 所谓的"语言"指的是GPL(通用目的语言, General Purpose Language). 与之相对的是DSL(领域特定语言, Domain Specific Language). 比如, SQL, HTML和
JSON就是DSL, 它们分别只能查询数据库, 渲染网页和储存数据, 除此以外几乎什么都不能做.

S-表达式能表示所有语义, 因此Lisp非常适合构建DSL. 比如你可以把SQL写成这样:

@codeblock{
(select *
    (from user)
    (where (between id 20220900 20220999)))
}

你不需要写一个新的parser, 因为这是一个可以被直接加载的S-表达式. 只要你坚持使用S-表达式, 在宏的帮助下, 就可以构造出一切语法. 这就是"从心所欲, 不逾矩".

所以, 面向语言编程就是: 分析具体问题, 设计一种DSL, 写出它的宏和函数库, 然后用它进行编程. 这可以避免一般语言中所谓"设计模式", 尤其适用于大型项目.

为了方便地管理一个语言中的宏和函数, Racket提供了#lang标记. 对于一般的#lang语言, 它的效果和require对应模块无异.

然而, 总有些人偏爱奇奇怪怪的语法, 也有些任务用S-表达式写出来不方便. 所以Racket提供了Reader Extension. 实现了这个特性的语言会用自己的reader解析源代码. 
比如PyOnR(Python on Racket)就是一个例子. 安装了PyOnR后, 你可以写出这样的程序:

@racketmodfile["pyonr.py"]

又比如, 我用的文档生成工具就是一个#lang语言. 它是DSL的一个典型例子, 除了生成文档几乎什么也都不能做.

为了鼓励构建自己的DSL, Racket在标准库了包括了LALR库parser-tools. 构建自己的语言是一项充满趣味的活动, 不过它不会在本文被涉及.