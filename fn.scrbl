#lang scribble/manual

@(require scribble/eval "util.rkt")

@title{Conditions and Procedures}

@cursor["Cursor"]{
With functions pure and high order too,
Our code is clean and concise, thanks to you.
}

@section{分支}

分支结构通过@racket[if]实现. 如下:

@examples[
    (if (= 1 1) "equal" "not equal")
]

if是惰性求值的, 因此

@examples[
    (if #f (/ 1 0) "")
]

不会报错.

if有许多派生语法, 包括@racket[when], @racket[unless], @racket[case], @racket[cond], @racket[and]和@racket[or].

when和unless都是字面意思. case相当于C中的switch:

@examples[
    (case 1
    [(1 2 3)
        'less-then-4]
    [else
        'greater-then-4])
]

cond相当于嵌套if:

@examples[
    (cond
    [(< 1 4)
        'less-than-4]
    [(= 1 4)
        'is-4]
    [else
        'greater-than-4])
]

and和or本质上也是if:

@examples[
    (syntax->datum (expand '(and 1 2 (or 3 4 5))))
]

还有一种比较复杂的分支结构-@racket[match]. 它的用法将不被介绍.

@section{函数}

Lisp是基于lambda演算的语言. Python的lambda和Lisp的lambda十分相似, 但Python里无返回值的语句过多, 导致lambda的功能受到了许多限制.

Racket的lambda支持默认参数和关键字参数, 不过我暂时不会介绍它们. 基础的lambda语法如下:

@examples[
    (define succ (lambda (n)
        (+ n 1)))
    (succ 1)
]

函数体内可以包含多个表达式. 调用时, 它们会被顺序求值, 只有最后一个会被返回:

@examples[
    ((lambda ()
        (displayln 'returning-42)
        42))
]

这在处理副作用(比如向标准输出打印)时有用. 如果你想在其他地方进行顺序求值, 使用begin:

@examples[
    (begin
        (displayln 'returning-42)
        42)
]

lambda后的括号是参数表. 当succ被调用时, 1被绑定(Bind)到了参数n, 最终结果是2.

lambda支持变长参数:

@examples[
    (define foo (lambda (a . b)
        (printf "~a ~a\n" a b)))
    (define bar (lambda args
        (displayln args)))
    (foo 1 2 3)
    (bar 1 2 3)
]

参数表所做的实际上是简单的模式匹配. 事实上, +, -等函数的参数就是不定的.

apply函数可以将列表解包至函数参数. 列表类型将在@secref["Lists"]和@secref["quote"]两章中具体介绍.

@examples[
    (apply + '(1 2 3))
]

由于函数定义十分常用, Racket提供了一种被称为"defun"的语法糖:

@examples[
    (define (succ n)
        (+ n 1))
]