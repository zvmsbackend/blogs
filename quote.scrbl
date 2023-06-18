#lang scribble/manual

@(require scribble/eval "util.rkt")

@title{quote}

@cursor["Cursor"]{
With a single quote, we can denote,
A symbol, a list, or a note.
}

前已述及, 在Lisp中程序即数据. 将程序转化为数据的方法叫@racket[quote].

@codeblock{
> (quote a)
'a
}

在这里我们quote了符号a, 使之不再发挥作为变量的功能, 这就是@secref["Basic_Grammar"]中提到的symbol和变量的关系. 可以看到, 它的效果与'a相同. 
事实上, 'a就是(quote a)的缩写.

如果你很无聊的话, 可以quote常量:

@examples[
    '"啊啊啊"
]

那么quote列表呢?

@codeblock{
> (quote (1 2 3))
'(1 2 3)
}

效果和'(1 2 3)是一样的. 也就是说, 列表是函数调用等树形语法结构的数据形式. 其实, Lisp正是LISt Process的缩写.

@examples[
    '(+ (+ 1 2) 3)
]

在这个例子中, 我们发现嵌套的结构是不会求值的. 如果想让一个quote中的部分求值, 使用@racket[quasiquote]:

@codeblock{
> (quasiquote (+ (unquote (+ 1 2)) 3))
'(+ 3 3)
}

还可以将一个列表插入quasiquote, 使用@racket[unquote-splicing]:

@codeblock{
> (quasiquote (max (unquote-splicing '(1 2 3))))
'(max 1 2 3)
}

Racket也提供了缩写:

@table['("全写" "缩写")
    '("quasiquote" "`")
    '("unquote" ",")
    '("unquote-splicing" ",@")]

@examples[
    `(+ ,(+ 1 2) 3)
    `(max ,@'(1 2 3))
]

到这里为止, 你已经基本掌握了把程序转化为数据的方式. 也许你以为这只是用来方便地初始化列表的罢了; 但它真正的用途, 即将数据转化为程序, 将在下一章讲述.