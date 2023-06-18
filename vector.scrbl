#lang scribble/manual

@(require scribble/eval "util.rkt")

@title{Vectors, Strings and Structures}

@cursor["Cursor"]{ 
Immutable or mutable, take your pick  
Easy to define, no need to click  
}

@section{向量与字符串}

@subsection{向量}

原则上, 函数式语言不支持数组. 但Scheme是多范式语言, 提供了向量(Vector)类型. 它绝非数学中的向量, 也和std::vector不同. 事实上, 它更接近System.Array.

下面演示了一些向量操作:

@examples[
    (define v (vector 1 2 3))
    v
    (vector-length v)
    (vector-ref v 0)
    (vector-set! v 0 0)
    v
    (make-vector 5 1)
]

也可以像用'创建列表那样创建向量, 不过符号要换成#.

@examples[
    (define v #(1 2 3))
    v
    (vector-set! v 0 0)
]

用#创建的向量是不可变(Immutable)的.

@margin-note{
如果你是Python list的重度用户的话, 注意: Racket标准库中没有变长数组.
}

@subsection{字符串}

和Haskell这样纯正的函数式语言不同, Racket的字符串是基于向量实现的. 它的用法和向量十分相似.

@examples[
    (define s (string #\1 #\2 #\3))
    s
    (string-length s)
    (string-ref s 0)
    (string-set! s 0 #\0)
    s
]

用@racket[string]函数创建字符串时, 每个参数都必须是字符. 可以用双引号创建字符串, 但这样创建的字符串和用#创建的向量一样是不可变的.

@margin-note{
Racket中的字符串字面量是可以跨行的, 这点与Python三引号字符串相似.
}

@section{结构体}

除了用来储存大规模数据以外, 向量还可以用来当作结构体. 比如, 你想表达一颗中缀表达式中的节点时, 就可以使用长度为3的向量:

@examples[
    (define (calc n)
        (if (vector? n)
            ((vector-ref n 1) (calc (vector-ref n 0)) (calc (vector-ref n 2)))
            n))
    (calc (vector 1 + 1))
]

这样做有些繁琐, 所以Racket提供了@racket[struct]. 它的底层是向量.

@examples[
    (struct node (left op right))
    (define (calc n)
        (if (node? n)
            ((node-op n) (calc (node-left n)) (node-right n))
            n))
    (calc (node 1 + 1))
]

结构体默认不提供setter函数, 这意味着它是不可变的.

结构体类型的创建支持继承和许多选项, 比如@racket[#:mutable]选项使结构体能提供setter函数. 这一部分留与读者自己探索.

Racket是动态类型的, 这就意味着struct可以实现其他语言中DU或ADT的功能. 它也支持模式匹配.

@subsection{泛型接口}

如果你研究了结构体的继承语法, 并知道面向对象的话, 你可能想像在Java里一样定义抽象方法和虚方法. Racket, 作为多范式语言, 提供了面向对象系统. 但这超出了本文的讨论范围; 另一边, struct也提供了一套多态(Polymorphism)机制, 和面向对象一样强大, 而且更加函数式. 它被称为泛型接口(Generic Interface).

@examples[
    (require racket/generic)
    (define-generics control
        (draw control))
]

@margin-note{
切记require racket/generic模块.
}

要使用GI, 在struct中使用@racket[#:methods]选项:

@examples[
    (require racket/generic)
    (define-generics control
        (draw control))
    (struct label (msg)
        #:methods gen:control
        [(define (draw control)
            (displayln (label-msg control)))])
    (define lbl (label "嘎嘎嘎"))
    (draw lbl)
]

使用@racket[define/generic]以区分为某个struct实现的方法和对所有实例适用的方法:

@examples[
    (require racket/generic)
    (define-generics control
        (draw control))
    (struct label (msg)
        #:methods gen:control
        [(define (draw control)
            (displayln (label-msg control)))])
    (define lbl (label "嘎嘎嘎"))
    (struct panel (controls)
        #:methods gen:control
        [(define/generic super-draw draw)
        (define (draw control)
            (for-each super-draw (panel-controls control)))])
    (define pnl (panel (list lbl)))
    (draw pnl)
]

GI实际上就是其他语言中的trait或typeclass.