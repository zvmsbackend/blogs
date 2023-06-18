#lang scribble/manual

@(require scribble/eval "util.rkt")

@title{Cursor's Racket Tutorial}

@centered{这是Cursor先生写的一篇Racket教程(中文版)}

@bold{Racket教程}

Racket是一种基于Scheme的编程语言，它是一种功能强大的语言，可以用于各种用途，包括Web开发、数据分析和游戏开发。在本教程中，我们将介绍Racket的基础知识，包括语法、数据类型、函数和模块等。

@section{安装Racket}

要开始使用Racket，您需要先安装它。您可以从[Racket官网](https://racket-lang.org/)下载适用于您的操作系统的安装程序。安装程序将指导您完成安装过程。

@section{Racket语法}

Racket的语法与Scheme非常相似。它使用括号表示函数调用和表达式。例如，下面是一个简单的Racket程序，它将两个数字相加：

@examples[
    (+ 1 2)
]

在这个例子中，+是一个函数，它接受两个参数，即1和2。函数名和参数之间用空格分隔。

Racket还支持定义变量和函数。例如，下面是一个将两个数字相加的函数的定义：

@examples[
    (define (add x y)
        (+ x y))
]

在这个例子中，define关键字用于定义一个函数。函数名是add，它接受两个参数x和y。函数体是一个表达式，它将x和y相加并返回结果。

@section{Racket数据类型}

Racket支持许多不同的数据类型，包括数字、字符串、布尔值和列表等。

@subsection{数字}

Racket支持整数和浮点数。例如，下面是一些数字的示例：

@examples[
    123
    3.14
]

@subsection{字符串}

Racket使用双引号表示字符串。例如，下面是一个字符串的示例：

@examples[
    "Hello, world!"
]

@subsection{布尔值}

Racket有两个布尔值：#t表示真，#f表示假。

@subsection{列表}

列表是Racket中最常用的数据类型之一。列表是一组值，可以包含任何类型的值。例如，下面是一个列表的示例：

@examples[
    '(1 2 3 4)
]

在这个例子中，'用于引用一个列表。列表中的值用空格分隔。

@section{Racket函数}

函数是Racket中的重要概念。函数是一段代码，它接受一些输入（参数），并根据这些输入执行一些操作。函数可以返回一个值，也可以不返回任何值。

@subsection{定义函数}

要定义一个函数，您可以使用define关键字。例如，下面是一个将两个数字相加的函数的定义：

@examples[
    (define (add x y)
        (+ x y))
]

在这个例子中，define关键字用于定义一个函数。函数名是add，它接受两个参数x和y。函数体是一个表达式，它将x和y相加并返回结果。

@subsection{调用函数}

要调用一个函数，您可以使用函数名和参数列表。例如，要调用add函数，您可以这样做：

@codeblock{
> (add 1 2)
3
}

在这个例子中，add函数接受两个参数1和2，并返回它们的和。

@section{Racket模块}

模块是Racket中组织代码的一种方式。模块是一组相关的函数和变量，它们可以被其他代码引用和使用。

@subsection{定义模块}

要定义一个模块，您可以使用module关键字。例如，下面是一个名为my-module的模块的定义：

@racketmodfile["my-module.rkt"]

在这个例子中，module关键字用于定义一个模块。模块名是my-module，它提供了一个函数add。函数add的定义与前面的例子相同。

@subsection{导入模块}

要使用另一个模块中的函数或变量，您需要先导入该模块。要导入一个模块，您可以使用require关键字。例如，要使用my-module模块中的add函数，您可以这样做：

@racketmodfile["require-my-module.rkt"]

在这个例子中，require关键字用于导入my-module模块。然后，我们可以使用add函数来计算1和2的和。

@section{结论}

在本教程中，我们介绍了Racket的基础知识，包括语法、数据类型、函数和模块等。希望这个教程能够帮助您开始使用Racket编程。