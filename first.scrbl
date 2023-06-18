#lang scribble/manual

@(require "util.rkt" scribble/eval)

@title{First Racket Program}

打开你喜欢的编辑环境, 输入:

@racketmodfile["first.rkt"]

再在终端输入(假定文件名为hello.rkt):

@codeblock{
$ racket hello.rkt
}

这样你的屏幕上就会出现这句古老的问候语.

你可能想在REPL(Read-Eval-Print Loop)环境中执行它, 就像在Pyshell和IPython里一样. 让我们直接执行racket:

@codeblock{
Welcome to Racket v8\.8 [cs]\.
> 
}

"> "说明你进入了REPL环境. 接着输入@racket[(enter! (file "hello.rkt"))].

当然你也可以直接在REPL里输入表达式并执行. 事实上你可以简单地在提示符后面输入@racket["hello, world"]然后得到结果:

@examples[
    "hello, world"
]

Racket还可以通过Racket自带的工具raco被编译为可执行程序. 具体方法不再展开.

让我们来解析一下第一个程序.

第一行的#lang racket声明这个文件使用racket语言. 你可能会好奇, 在racket上除了racket还有什么语言呢? 事实上, Racket支持面向语言编程, 这个概念将在后文被解释.

第二行是注释. 在Racket程序中, 分号;直到行尾的内容被视为注释. 另外, 被#||#包围的是多行注释. 与C的/**/不同, Racket多行注释是可嵌套的.

第三行调用了内建displayln函数, 相当于C的puts, 但支持任意参数类型. 与大多数语言不同, Lisp的函数调用中函数在括号里面, 参数之间以空格分隔. 
事实上, 除了函数调用以外, 任何Lisp语法都可以被写成这种形式. 这种以括号标记的语法称为S-表达式. 