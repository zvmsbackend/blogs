#lang scribble/manual

@(require scribble/eval "util.rkt")

@title{High-Order Functions}

@cursor["Cursor"]{
Compose and curry, map and filter
Abstractions that make our code much neater
}

在函数式和多范式语言中, 函数是一级对象(First-Class Object), 和其他类型的值一样可以被传递. 所谓高阶函数, 并非数学里的高阶函数, 而是指参数是函数, 或返回值是函数的函数.

最常见的高阶函数是@racket[map], @racket[filter]和@racket[fold]. 前两者在Python中都有对应. 至于fold, Python提供了functools.reduce, 名字不一样, 功能却相同.

@section{map}

map函数接受一个函数和一个列表, 将列表中所有元素用函数处理后返回到另一个列表中. 在LINQ中它又叫Select.

@examples[
    (map add1 '(1 2 3))
]

我们可以这么实现它:

@racketmodfile["map.rkt"]

内置的map函数实际上可以接受任意数量的列表:

@examples[
    (map cons '(1 2 3) '(4 5 6))
]

至于如何实现, 留与读者作为练习.

@section{filter}

filter函数接受一个谓词和一个列表, 将列表中符合谓词的元素收集到一个新的列表后返回. 在LINQ中它又叫Where.

@examples[
    (filter odd? '(1 2 3))
]

我们可以这么实现它:

@racketmodfile["filter.rkt"]

@section{fold}

fold函数接受一个函数, 一个初始值和一个列表, 将列表中的元素依次通过函数施加到初始值上, 最后将其返回. 在LINQ中它又叫Aggregate.

@examples[
    (foldl + 0 '(1 2 3))
]

注意这里的函数名为@racket[foldl], l取left意. 与之相对的是@racket[foldr], 将列表逆序fold. 我们可以这么实现foldl:

@racketmodfile["fold.rkt"]

fold用处多多, 比如reverse函数就可以用fold实现:

@racketmodfile["reverse.rkt"]

上述三个函数都是接受函数的高阶函数. 返回函数的高阶函数主要有@racket[curry], @racket[curryr], @racket[compose], @racket[conjoin], @racket[disjoin]等. 
它们也十分强大, 但在本文不作详述