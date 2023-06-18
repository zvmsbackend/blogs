#lang scribble/manual

@(require scribble/eval "util.rkt")

@title{Recursion}

@cursor["Cursor"]{
From trees to graphs, and lists in between,
Recursion solves them all, it's a dream!
}

作为函数式语言, Racket里没有while循环. 倒是有一些类似循环的结构(比如for迭代), 不过它们的底层都是递归.

递归函数的定义和普通函数没有什么区别:

@examples[
    (define (fib n)
        (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2)))))
    (fib 10)
]

如果你想定义局部递归函数, 不能用let, 要用@racket[letrec]:

@examples[
    (letrec ([fib (lambda (n)
        (if (< n 2)
            n
            (+ (fib (- n 1)) (fib (- n 2)))))])
        (fib 10))
]

因为局部定义-调用的模式非常普遍, Racket提供了一种语法糖:

@examples[
    (let loop ([n 10])
        (if (< n 2)
            n
            (+ (loop (- n 1)) (loop (- n 2)))))
]

把递归函数命名为loop是一种惯用法.

你可能被告知过, 相比循环, 递归的时空效率低下. 这是真的. 为了避免额外的开销, 函数式语言们采用了一种被称为"尾递归优化"的技术. 简而言之, 如果函数体里的最后一个表达式是对自己的递归调用, 该递归会被优化为循环.

使得尾递归优化成为可能的模式叫累加器(Accumulator)模式. 理论上说, 一切基于循环的算法(包括递推)都可以被转化为尾递归形式.

考虑while循环求区间和的Python实现:

@racketmodfile["sum.py"]

在Racket里可以这么写:

@racketmodfile["sum.rkt"]

你现在应该知道函数式编程不需要set!的原因了.

因为这种模式也十分常见, Racket提供了@racket[do]构造:

@racketmodfile["do.rkt"]

递归还可以用来高效地处理列表:

@examples[
    (define (product ls)
        (let loop ([acc 1] [ls ls])
            (cond
            [(null? ls) acc]
            [(= (car ls) 0) 0]
            [else (loop (* acc (car ls)) (cdr ls))])))
    (product '(1 2 3))
]