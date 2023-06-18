#lang scribble/manual

@(require scribble/eval "util.rkt")

@title{Macros}

@cursor["Cursor"]{
Thou art the heart of Lisp, the soul of its power,
With thy ability to extend, we can make it flower.
}

所有Lisp程序都是可以被转化为列表的, 这也是Lisp把函数放在括号里面的原因. 这种看似古怪的语法使得列表可以被轻易地转化为程序. 如果你了解Python或JS的话, 
可能会想到eval函数. Racket的确提供了@racket[eval]函数, 但这通常不是把数据转化为程序的方式. 宏(Macro)是主要的方式, 也是Lisp的核心.

如果你学过C的话, 应该对宏比较熟悉, 但两个宏的能力简直是天壤之别. 之所以C的宏那么弱, 是因为它只进行简单的文本替换, 而这既低效又麻烦. Lisp宏本质上也是
程序片段的替换(即元编程, Meta Programming), 但其能力是C宏所远不可即的. 两者的核心差别是: Lisp并不真正区分编译时和运行时, 所谓的宏就是一个接受S-表达式, 
返回S-表达式的函数. 它是能实现"编译时"计算的.

Scheme(包括Racket)的宏与传统Lisp宏有一些区别. 它们不操作列表形式的S-表达式, 而是操作syntax-object. syntax-object包含了源代码位置等元数据, 除此以外和
列表等类型十分相似.

@examples[
    (syntax a)
    #'(+ 1 1)
    #`(+ #,(+ 1 2) 3)
]

宏通过@racket[define-syntax]定义(这个词是不是有些熟悉?):

@examples[
    (require racket)
    (define-syntax (senpai . args) #'114514)
    senpai
]

看起来就是C的#define senpai 114514. 但它们实际上是有区别的:

@examples[
    (require racket)
    (define-syntax (senpai . args) #'114514)
    (114514)
    (senpai)
]

怎么回事呢? 让我们看一看这个函数收到的参数:

@examples[
    (require racket)
    (define-syntax (senpai . args)
        (writeln args)
        #'114514)
    (senpai)
]

原来senpai这个符号被绑定关键字(Bind Keyword)了. 当Macro Expander遇到符号senpai或car为senpai的结构时, 它就会把syntax-object传给对应的函数.

如此宏的工作原理就很清晰了: Macro Expander把程序转化为数据后传给宏, 宏把数据数量后转化会程序再插回去. Common Lisp等Lisp方言中的宏就是这么做的.

Racket中你也可以这么做, 但这样比较麻烦; 还有一个问题在于名字污染. 考虑下面的C宏:

@racketmodfile["swap.c"]

然后这么调用:

@racketmodfile["call.c"]

你会得到一个编译错误.

为了解决这些问题, Scheme封装了一些基于模式匹配的宏创建方式, 其中最方便的是@racket[syntax-rules]. 比如, 我们可以把when构造用宏来实现:

@examples[
    (define-syntax when (syntax-rules ()
    [(_ test body ...)
        (if test
            (begin
                body ...)
            (void))]))
    (when #t
        (display "hello, ")
        (display "world"))
]

上面的_是通配符(Wildcard), 匹配任意模式而不捕获. 因为car必然是when, 所以我们使用通配符.

...代表零个或多个模式, 相当于正则里的*. 注意它是一个单独的标识符, 不可与其他单词连在一起.

syntax-rules后面的()的内容是字面值(Literal), 在when的例子里没有用到. 如果要举例的话, cond和case(它们也是宏)中的else就是literal. 具体的用法可以参考
#hyperlink["#" "官方导引"]

很多时候宏只有一种模式, 因此编译器允许你这么写:

@racketmodfile["define-syntax-rule.rkt"]

此外, Racket宏还支持递归, 这使得它能处理任意情况的语法. 比如, 我们可以把or用宏实现:

@examples[
    (define-syntax my-or (syntax-rules ()
        [(_) #t]
        [(_ e) e]
        [(_ e1 e2 e3 ...)
            (let ([t e1])
                (if t
                    t
                    (my-or e2 e3 ...)))]))
    (let ([t 1])
        (my-or t #f 2))
]

除了syntax-rules以外, Racket宏还有许多高级功能, 比如fender, identifier等, 在此不一一赘述.