#lang scribble/manual

@(require scribble/eval "util.rkt")

@title{Basic Grammar}

@section{基础数据类型和运算符}

Racket的基础数据类型包括boolean, number, character和symbol等.

@subsection{布尔类型}

Racket使用@racket[#t]和@racket[#f]来表示boolean类型. 它们相当于Python中的True, False和bool. not函数含义与Python的同.

与Python不同的是, 在Racket中除了#f以外的值都被是为真.

@examples[
    (not #f)
    (not 0)
]

@subsection{数字类型}

Racket提供了非常丰富的数字类型. 包括整数, 实数, 有理数和复数:

@examples[
    (number? 42)
    (integer? 42)
    (rational? 1/2)
    (real? 114.514)
    (complex? 1+2i)
]

@margin-note{
实数相当于Python的浮点数, 有理数相当于fraction.Fraction. 复数使用字母i而非j. 另外, Racket的整数和Python的一样, 是没有长度上限的.
}

Racket还提供了一系列运算符:

@examples[
    (+ 1 1)
    (* 1 2 3)
    (= 1 1.0)
    (>= 100 50)
]

可以发现, 这些运算符的调用方法和函数是一样的. 事实上, 这些运算符@bold{就是}函数. Racket并没有提供特殊的中缀运算符语法.

@margin-note{
Python的**对应expt函数, //对应quotient, %对应modulo和remainder. modulo和remainder都是用来取模的, 但在处理负数时略有区别.
}

此外还有一些实用函数:

@examples[
    (max 1 1 4 5 1 4)
    (min 1 9 1 9 8 1 0)
    (abs -1)
]

Racket的全部数学函数可以在@hyperlink["#" "官方参考"]中找到.

@subsection{字符类型}

如果你学过C, 那么应该知道字符与字符串的区别. Racket的字符不用但引号包围, 而是用#\前缀标注.

@examples[
    #\a
    #\newline
    #\space
]

@subsection{符号类型}

符号类型通过'创建. 关于它的详细论述将在@secref["quote"]一节.

@examples[
    'I-am-a-symbol
    '|I'm-a-symbol-too|
]

下面的|将里面的'转义, 使之能被放在符号中.

@section{变量}

变量在Racket中又称作符号(Symbol). Racket使用@racket[define]构造定义一个变量:

@examples[
    (define foo 114514)
    foo
]

你可能会感到奇怪: symbol不是一种数据类型吗? 不要忘了, 在Lisp中, 程序即数据. 它们之间的关系将在@secref["quote"]和@secref["Macros"]中揭晓.

Lisp的符号命名规则十分宽松. 在转义字符的帮助下, 你可以将任意字符串转化为symbol. 这里将不会介绍命名的细则, 但了解Scheme的基本命名规范将是有帮助的:

@table['("字符" "含义" "例子")
    (list "_" "分隔单词" @racket[call-with-current-continuation])
    (list "?" "谓词" @racket[null?])
    (list "!" "副作用" @racket[set!])
    (list "/" "缩写/变体" @racket[call/cc for/list])
    (list "*" "增强" @racket[let*])]

要想修改foo, 使用@racket[set!]构造:

@examples[
    (define foo 114514)
    (set! foo 1919810)
    foo
]

这种行为在函数式编程中是要尽量避免的. 替代set!的方法将在@secref["Recursion"]一节中介绍

局部变量用let定义:

@examples[
    (let ([a 1]
        [b 2])
        (+ a b))
    a
]

这里a和b的作用域仅限于let构造的内部.