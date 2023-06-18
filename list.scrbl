#lang scribble/manual

@(require scribble/eval "util.rkt")

@title{Lists}

@cursor["Cursor"]{
From head to tail, a path so clear,
Traversing thee brings nary a fear.
}

列表(List)是Lisp中最重要的数据结构. 与Python中的list不同, Lisp的列表是单向链表而非数组. 一个列表节点可能有两种取值: null或pair.

pair又叫点对(Dotted Pair), 只有两个字段, 称为car和cdr. 在列表中, car指向当前节点的数据, cdr指向下一个节点. 一个列表的最后一项必须是null, 否则它被称为improper list.

pair使用cons(CONStruct)函数创建:

@examples[
    (cons 1 2)
    (cons 1 (cons 2 null))
]

第二个点对的表示中没有点, 说明它是一个列表. 为了方便地构建列表, Racket提供了list函数:

@examples[
    (list 1 2)
]

你还可以用类似于创建symbol的方法更快捷地创建列表:

@examples[
    '(1 2)
]

同样, 它的原理将在@secref["quote"]中介绍.

要判断一个值是否为列表, 使用@racket[pair?]和@racket[null?]

@examples[
    (define l (list 1))
    (pair? l)
    (null? l)
    (null? (cdr l))
]

你可以用@racket[length]和@racket[list-ref]函数获取列表长度或进行随机访问:

@examples[
    (define l '(1 2 3 4))
    (length l)
    (list-ref l 1)
]

记住: Lisp list是链表, 因此这种访问方式是十分低效的. 操作列表的正确方式将在@secref["Recursion"]和@secref["High-Order_Functions"]中介绍.

另外, 点对与列表是不可变的, 这与std::forward_list的默认行为不同. 

Racket提供了一些有用的列表操作函数, 比如append, reverse和sort. 还有一些函数将出现在@secref["High-Order_Functions"].

为了简化列表访问, Racket提供了一些简写:

@examples[
    (define l '((1 2 3) 4 5 6))
    (caar l)
    (caddr l)
]

"cadnr"风格的简写只支持到四级.