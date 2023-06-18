#lang scribble/manual

@(require "util.rkt")

@title{The Hy Programming Language}

@centered{本文译自@hyperlink["#" "docs.hylang.org"].}

Hy是一个嵌入Python的Lisp方言. 由于Hy被转化为Python抽象语法树(Abstract Syntax Tree, AST)对象,
 你可以以Lisp的形式拥有整个美丽的Python世界.

为了下载最新版的Hy, 只需键入命令@code{pip install hy}, 
接下来你可以用命令@code{hy}打开互动读取-求值-打印循环
(Read-Eval-Print Loop, REPL), 
或用@code{hy myprogram.hy}运行一个Hy程序.

Hy在所有CPython的发布和维护版本(在Linux和Windows上), 
以及目前的PyPy和Pyodide版本@smaller{
(译注: PyPy是一个支持即时编译(Just-in-Time, JIT)
技术的快速Python版本; Pyodide是一个运行在浏览器上的Python版本)}上都经过测试.

@local-table-of-contents[]

@section{什么是Hy?}

Hy(或长名"Hylang", 以Hymenoptera@smaller{(译注: 
膜翅目, 包括蜂, 蚂蚁等昆虫)}命名, 因Paul Tagliamonte@smaller{
(译注: Hy项目的发起者)}在创造这个语言时正在研究昆虫行为)是一个多范式
(Multi-Paradigm)通用目的(General-Purpose)
Lisp家族的程序语言. 它被实现为一种Python的替代语法. 
与Python相比, Hy提供了一系列Lisp中有的额外特性, 
泛化和语法简化. 与其他Lisp相比, Hy能直接访问Python的内建和第三方库, 
并允许你自由地混合命令式(Imperative), 函数式(Functional)
和面向对象(Object-Oriented)的编程风格.

@subsection{Hy与Python}

Python程序员会注意到的第一件事是Lisp的传统前缀括号语法代替了Python的C风格中缀语法. 
比如, @code{print("The answer is", 2 + object.method(arg))}
在Hy中被写作@code{(print "The answer is" (+ 2 (.method object arg)))}. 
所以, Hy是格式自由的: 结构由标点而非缩进表示, 使之易于命令行使用.

像其他Lisp一样, 这种简单语法的价值在于使Lisp的标志性功能, 
用宏(Macro)进行元编程(Metaprogramming)成为可能. 
宏是用于在编译时操作代码对象, 产生新的代码对象插入源代码的函数. 实际上, 
Hy允许任意编译期计算. 比如, 下面是一个实现C风格
@code{do-while}循环的简单宏, 它在条件为真时执行循环体, 
至少执行一次.

@pyblock{
(defmacro do-while [condition #* body]
  `(do
    ~body
    (while ~condition
      ~body)))
(setv x 0)
(do-while x
  (print "This line is executed once."))
}

Hy也移除了Python对混用语句和表达式的限制, 允许更直接和函数式的代码. 
比如, Python不允许@code{with}块(在使用资源后关闭之)返回值, 
它只能执行一系列语句:

@pyblock{
with open('foo') as o:
    f1 = o.read()
with open('bar') as o:
    f2 = o.read()
print(len(f1) + len(f2))
}

在Hy中, with返回其最后一个form, 所以你可以这么写, 就像一个普通的函数调用:

@codeblock{
(print (+
  (len (with [o (open "foo")] (.read o)))
  (len (with [o (open "bar")] (.read o)))))
}

为了更加简洁, 你可以把@code{with}用在@code{gfor}中:

@codeblock{
(print (sum (gfor
  filename ["foo" "bar"]
  (len (with [o (open "foo")] (.read o))))))
}

此外, Hy还提供了Python二元运算符的一些泛化. 运算符可以接受多于两个的参数
(如@code{(+ 1 2 3)}), 包括增强赋值运算符(如@code{(+= x 1 2 3)}). 
它也提供了一些同名的一等(First-Class)函数, 可以被传递给高阶函数(High-Order Function): 
从模块@code{hy.pyops}导入函数@code{+}后, @code{(sum xs)}
可被写作@code{(reduce + xs)}.

Hy编译器将Hy源码读取为Hy model@smaller{(译注: 
找不到能准确地反映"model"意思的中文词汇, 故保留原文. 下文form同理)}对象, 
再将其编译为Python抽象语法树(@code{ast})对象. 
@code{ast}对象可以被Python解释器编译运行, 或编译为字节码以供快速调用, 
或被渲染为Python源码. 你甚至可以将Python和Hy在同一项目, 
乃至同一文件中混用, 这是使你熟悉Hy的好方法.

@subsection{Hy与其他Lisp}

在运行时, Hy@bold{就是}Python代码. 所以, 虽然Hy的设计很大程度上借鉴了Clojure
@smaller{(译注: 一种编译到Java字节码的Lisp方言)}, 
它与Python的关系比Clojure与Java的关系更紧密; 一个更好的类比是CoffeeScript
@smaller{(译注: 一种编译至JavaScript的语言)}和Javascript的关系. 
Python的内建函数和数据类型是直接可用的.

@codeblock{
(print (int "deadbeef" :base 16))  ; 3735928559
(print (len [1 10 100]))           ; 3
}

PyPI上或其他地方的的Python库也一样. 下面是Hy中的一个简单CherryPy
@smaller{(译注: 一个极简的Python Web框架)}Web应用:

@codeblock{
(import cherrypy)

(defclass HelloWorld []
  (defn [cherrypy.expose] index [self]
    "Hello World!"))

(cherrypy.quickstart (HelloWorld))
}

你甚至可以在PyPy上运行Hy以求速度.

与所有Lisp一样, Hy是同像(Homoiconic)的@smaller{(译注: 指程序和数据采用同样的表示法)}. 
它的语法不以cons cell@smaller{(译注: 其他Lisp中的常见数据形式)}
或Python基础数据结构, 而用称作model的Python基础类型的简单子类表示. 
使用model而非普通@code{list}或@code{set}有两个目的: model可追踪其在源码中的行号和列号, 
便于生成错误信息. 并且model可提供对应类型没有的特性,比如集合字面量中的元素顺序. 
然而, model可以像普通列表一样拼接和索引, 并且你可以从宏中返回普通Python类型或把它传给
@code{hy.eval}. Hy会自动把它们提升(Promote)为model. 

Hy从Python吸收了很多语义. 比如, Hy是一种Lisp-1, 
因为Python的函数和非函数对象使用相同的命名空间@smaller{
(译注: 在一些Lisp方言, 比如Common Lisp中, 函数使用单独的命名空间. 
它们被称作Lisp-2)}.总的来说, 任何Python代码都可以被字面翻译到Hy. 
同时, Hy允许你做一些在Python中没有的典型Lisp行为.

比如, Hy提供了前已述及的语句-表达式混用, 将@code{valid?}
这样的符号转化到Python合法标识符的名字转义(Mangle), 
并提供了代替Python函数级作用域的块级作用域(Block-Level Scoping)的宏@code{let}.

总之, Hy, 与Common Lisp一样, 致力于成为一个兼容并蓄的通用语言, 
让你做一切想做的事@smaller{(译注: 义工管理系统后端使用Hy语言编写)}. 
如果你对小巧优雅的Scheme风格Lisp感兴趣, 试试@hyperlink["#" "Hissp"], 
由一位Hy开发者创造的另一个嵌入Python的Lisp方言.

@section{教程}

本章提供了对Hy的快速介绍. 它假定读者的编程背景, 但不要求对Python和Lisp的知识.

@subsection{Python上的Lisp}

让我们用经典问候语开始:

@codeblock{
(print "Hy, World!")
}

这个程序调用了@code{print}函数. 像所有Python内建函数一样, 它在Hy中是直接可用的.

所有Python二元运算符也是可用的, 尽管@code{==}按照Lisp传统被拼作@code{=}. 
下面是加法运算符的例子:

@codeblock{
(+ 1 3)
}

这段代码返回4, 与Python等语言中的@code{1 + 3}等价. 包括Hy在内的Lisp语言使用前缀语法: 
@code{+}, 与@code{print}或@code{sqrt}一样, 出现在参数之前. 调用由括号决定, 
但左括号出现在调用的运算符之前而非之后, 所以要写@code{(sqrt 2)}而非@code{sqrt(2)}. 
多个参数, 如@code{(+ 1 3)}中的两个整数, 被空白分隔, 许多运算符, 包括@code{+}, 
允许多于两个的参数: @code{(+ 1 2 3)}与@code{1 + 2 + 3}等价.

下面是一个复杂一点的例子:

@codeblock{
(- (* (+ 1 3 88) 2) 8)
}

这段代码返回176. 为什么? 我们可以用命令
@pycode{echo "(- (* (+ 1 3 88) 2) 8)" | hy2py}看到等价中缀表达式. 
也可以在用@code{hy}打开REPL时传递@code{--spy}参数, 使Python等价在每个结果前被打印.
此表达式的中缀等价是:

@codeblock{
((1 + 3 + 88) * 2) - 8
}

求值这个中缀表达式时, 你当然会先计算最内部的括号表达式, 然后逐步向外扩展. 
Lisp也一样. 下面是逐步计算上文Hy代码的过程:

@codeblock{
(- (* (+ 1 3 88) 2) 8)
(- (* 92 2) 8)
(- 184 8)
176
}

与C或Python的表达式类似, Lisp的基本语法单位是form. @code{92}, 
@code{*}和@code{(* 92 2)}都是form. 一个Lisp程序是由一系列嵌套form构成的. 
form间一般以空白分隔, 但有些form, 如字符串字面量(@code{"Hy, World!"})
自身可包含空白. 一个表达式(Expression)是由括号包围的form; 它的第一个子form, 
称为头(Head), 决定了表达式的作用, 一般为一个函数或宏. 函数是最常见的头, 
而宏(会在后文详述)是在编译时允许并返回在运行时运行的代码的函数.

注释从;字符开始, 到行末为止. 注释与空白无异.

@codeblock{
(setv password "susan")   ; My daughter's name
}

虽然#不是Hy的注释符号, 但Hy程序可以Shebang@smaller{
(译注: Shebang是类Unix系统, 如Linux, MacOs下使文本文件能被直接执行的方法. 
Windows上无此功能)}开始. Hy会忽略它.

@codeblock{
#!/usr/bin/env hy
(print "Make me executable, and run me!")
}

@subsection{字面量}

Hy对于所有有字面量(Literal)的Python类型都有字面量语法. 下面是Hy与Python字面量语法的对应关系:

@table['("Hy" "Python" "类型")
    '("1" "1" "int")
    '("1.2" "1.2" "float")
    '("4j" "4j" "complex")
    '("True" "True" "bool")
    '("None" "None" "NoneType")
    '("\"hy\"" "'hy'" "str")
    '("b\"hy\"" "b'hy'" "bytes")
    '("#(1 2 3)" "(1, 2, 3)" "tuple")
    '("[1 2 3]" "[1, 2, 3]" "list")
    '("#{1 2 3}" "{1, 2, 3}" "set")
    '("{1 2  3 4}" "{1: 2, 3: 4}" "dict")]

Hy REPL默认用函数@code{hy.repr}输出:

@codeblock{
=> [1 2 3]
[1 2 3]
}

但如果你这样打开Hy:

@codeblock{
$ hy --repl-output-fn=repr
}

REPL会使用Python的内建@code{repr}函数, 所以你会见到Python的语法形式:

@codeblock{
=> [1 2 3]
[1, 2, 3]
}

@subsection{基本操作}

用@code{setv}设置变量:

@codeblock{
(setv zone-plane 8)
}

用@code{get}访问列表, 字典或其他数据结构的元素:

@codeblock{
(setv fruit ["apple" "banana" "cantaloupe"])
(print (get fruit 0))  ; => apple
(setv (get fruit 1) "durian")
(print (get fruit 1))  ; => durian
}

用@code{cut}访问有序结构中的一个范围内的元素:

@codeblock{
(print (cut "abcdef" 1 4))  ; => bcd
}

用@code{if}构造逻辑分支:

@codeblock{
(if (= 1 1)
  (print "Math works. The universe is safe.")
  (print "Math has failed. The universe is doomed."))
}

在这个例子中, @code{if}用@code{(if CONDITION THEN ELSE)}的形式调用. 
如果@code{CONDITION}为真(根据@code{bool}), 返回@code{THEN}, 
否则返回@code{ELSE}.

如果你想在@code{THEN}或@code{ELSE}子句, 
抑或@code{CONDITION}里使用多个form呢? 用@code{do}宏
(在传统Lisp中叫@code{progn}@smaller{(译注: 在Scheme里叫@code{begin})}). 
它将多个form合并为一个, 返回最后一个.

@codeblock{
(if (do (print "Let's check.") (= 1 1))
  (do
    (print "Math works.")
    (print "The universe is safe."))
  (do
    (print "Math has failed.")
    (print "The universe is doomed.")))
}

用@code{cond}处理多个分支:

@codeblock{
(setv somevar 33)
(cond
  (> somevar 50)
    (print "That variable is too big!")
  (< somevar 10)
    (print "That variable is too small!")
  True
    (print "That variable is jussssst right!"))
}

宏@code{(when CONDITION THEN-1 THEN-2 ...)}是
@code{(if CONDITION (do THEN-1 THEN-2 ...) None)}的简写.

Hy的基本循环有@code{while}和@code{for}:

@codeblock{
(setv x 3)
(while (> x 0)
  (print x)
  (setv x (- x 1)))  ; => 3 2 1

(for [x [1 2 3]]
  (print x))         ; => 1 2 3
}

更函数式的做法是用@code{lfor}推导式. @code{for}总是返回@code{None}, 
而@code{lfor}返回包含每次迭代结果的列表:

@codeblock{
(print (lfor  x [1 2 3]  (* x 2)))  ; => [2, 4, 6]
}

@subsection{函数, 类和模块}

用@code{defn}定义函数:

@codeblock{
(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))
(print (fib 8))  ; => 21
}

用@code{fn}定义匿名函数:

@codeblock{
(print (list (filter (fn [x] (% x 2)) (range 10))))
  ; => [1, 3, 5, 7, 9]
}

@code{defn}或@code{fn}参数表内的特殊符号允许你声明可选参数, 提供默认值, 
或收集未列出的参数:

@pyblock{
(defn test [a b [c None] [d "x"] #* e]
  [a b c d e])
(print (test 1 2))            ; => [1, 2, None, 'x', ()]
(print (test 1 2 3 4 5 6 7))  ; => [1, 2, 3, 4, (5, 6, 7)]
}

用:keyword给函数传递关键字参数:

@codeblock{
(test 1 2 :d "y")             ; => [1, 2, None, 'y', ()]
}

注意, 与Python不同, Hy不总是按代码中出现的顺序求值函数参数(或列表, 
字典字面量中的项). 但你总可以用@code{do}或提供了隐式@code{do}的宏, 
如@code{when}或@code{fn}, 来强制顺序求值.

用@code{defclass}定义类:

@codeblock{
(defclass FooBar []
  (defn __init__ [self x]
    (setv self.x x))
  (defn get-x [self]
    self.x))
}

下面我们创建了@code{FooBar}类的实例@code{fb}, 并用多种方式访问了它的属性:

@pyblock{
(setv fb (FooBar 15))
(print fb.x)         ; => 15
(print (. fb x))     ; => 15
(print (.get-x fb))  ; => 15
(print (fb.get-x))   ; => 15
}

注意, @code{fb.x}和@code{fb.get-x}这样的语法仅适用于被调用的对象
(此处为@code{fb})是一个简单变量名时. 用@pycode{(. FORM x)}或
@code{(.get-x FORM)}以获取任意form @code{FORM}的属性或调用其方法.

用@code{import}访问外部模块, 不管它是用Python还是Hy写的:

@codeblock{
(import math)
(print (math.sqrt 2))  ; => 1.4142135623730951
}

只要Hy自身被导入了, Python就可以导入任意Hy模块. 当然, 
如果你运行过一个Hy程序, 这已经发生过了.

@subsection{宏}

宏是Lisp的基本元编程工具. 宏是在编译时(如当一个Hy程序被翻译为Python 
@code{ast}对象时)被调用, 返回成为最终结果程序的部分的函数.

下面是一个简单的例子:

@codeblock{
(print "Executing")
(defmacro m []
  (print "Now for a slow computation")
  (setv x (% (** 10 10 7) 3))
  (print "Done computing")
  x)
(print "Value:" (m))
(print "Done executing")
}

如果你运行这个例子两次, 你大概会见到这个:

@codeblock{
$ hy example.hy
Now for a slow computation
Done computing
Executing
Value: 1
Done executing
$ hy example.hy
Executing
Value: 1
Done executing
}

"slow computation"在首次调用编译程序时执行. 整个程序被编译后, 
正常的执行才从头开始, 打印"Executing". 当程序被第二次调用时, 
它用先前编译的字节码运行, 与下面的等价:

@codeblock{
(print "Executing")
(print "Value:" 1)
(print "Done executing")
}

宏m返回了一个特别简单的值, 一个整数. 它在编译时被转化为一个整型字面量. 
一般地, 宏可以返回任意Hy form以为代码执行. 有几个帮助的宏简化了form在编程中的构造, 
如@code{quote}('), @code{quasiquote}(`), @code{unquote}(~)和
@code{defmacro!}. 上一章的简单例子用了`和~以定义新的控制结构@code{do-while}.

如果你想使用别的模块中定义的宏呢? @code{import}没用, 因为它被翻译为Python中的
@code{import}语句, 在运行时被调用, 而宏在编译时, 也就是在从Hy到Python的翻译中展开. 
应使用@code{require},它在编译时导入模块中的宏. @code{require}的语法和@code{import}的一致.

@codeblock{
=> (require tutorial.macros)
=> (tutorial.macros.rev (1 2 3 +))
6
}

Hy也支持读取宏. 它与普通宏相似, 但操作源码文本而非解析过的Hy form. 
它被调用时可以选择使用多少源码, 并返回任意代码. 所以, 
读取宏可以向Hy中加入全新的语法. 比如, 可以给Python的@code{decimal.Decimal}添加字面量语法:

@pyblock{
=> (import  decimal [Decimal]  fractions [Fraction])
=> (defreader d
...   (.slurp-space &reader)
...   `(Decimal ~(.read-ident &reader)))
=> (print (repr #d .1))
Decimal('0.1')
=> (print (Fraction #d .1))
1/10
=> ;; 与普通浮点 .1相比:
=> (print (Fraction .1))
3602879701896397/36028797018963968
}

@code{require}可以从其他模块中拉取宏, 用这样的语法: 
@code{(require mymodule :readers [d])}

@subsection{Hyrule}

Hyrule是Hy的标准实用库. 它提供了一系列在写Hy程序中有用的函数和宏:

@codeblock{
=> (import hyrule [inc])
=> (list (map inc [1 2 3]))
[2 3 4]
=> (require hyrule [assoc])
=> (setv d {})
=> (assoc d  "a" 1  "b" 2)
=> d
{"a" 1  "b" 2}
}

@smaller{(译注: 使用@code{pip install hyrule}下载)}

@subsection{下一步}

参考Python文档以学习Python语义的细节. @hyperlink["#" "the Python tutorial"]
尤其有用, 即使你对自己写Python程序不感兴趣. 因为它为你介绍语义, 
而你需要知道一定Python语法知识以理解Python库中的样例代码.

参考本文的余下部分以学习Hy专有的语法. 像Hy本身一样, 这篇文章是不完整的. 
但我们欢迎你的贡献.

记住, Hy仍是不稳定的, 并且每个Hy 1.0之前的新版本都可能带来革命性的更新. 
参考@hyperlink["#" "the NEWS file"]以了解如何在升级Hy的同时更新你的代码, 
并确保你的文档的版本@smaller{(译注: 本文为0.26.0)}与你运行的Hy版本匹配.

@section{语法}

本章描述了Hy源码是如何在文本层面上被理解的, 以及调用@code{hy.eval}时读取器
(Reader)(又叫解析器, Parser)是如何把文本转化为抽象语法树的. 
文本层面的基本语法单位叫form, 而表示form的基本单位叫model.

与Python相同, Hy是大小写敏感的@smaller{(译注: 
Common Lisp和早期版本的Scheme不是)}. 比如, @code{foo}和@code{FOO}是不同的符号, 
并且它们指向的Python变量是不同的.

@subsection{Model介绍}

读取Hy程序产生嵌套的model对象. model可能与它表示的值十分相似
(比如@code{Integer}是@code{int}的子类), 也可能有些不同
(比如@code{Set}是有序的, 与实际的@code{set}不同).所有model继承自@code{Object}, 
它保存了代码的文本位置信息, 所以栈回溯能指向代码中的正确位置. 
编译器收集解析和宏展开后剩下的model, 翻译至Python @code{ast}节点
(如@code{Integer}变成@code{ast.Constant}), 最后被执行或渲染为Python代码. 
宏(常规宏, 而非读取宏)在model层面操作, 以一些model为参数, 返回其他model以供编译或进一步展开; 
它可以做与编译器截然不同的事,比如根据@code{Integer}构造@code{Symbol}.

一般model与其表示的值不相等. 比如, @code{(= (hy.models.String "foo") "foo")}
返回@code{False}. 但你可以将一个值用@code{hy.as-model}提升为对应model, 
或用@code{str}, @code{int}等Python构造器将model降级, 
或用@code{hy.eval}求值一个model.

model可用构造函数, @code{quote}和@code{quasiquote}, 或@code{hy.as-model}创建. 
显示创建一般不必要, 因为编译器会将其求值的任意对象(通过@code{hy.as-model})自动提升.

注意, 当你想用POD(Plain Old Data)结构且不产生可运行的Hy源码时, 
最好用Python基础数据结构(@code{tuple}, @code{list}, @code{dict}等)
而非model. "同像性"是个有意思的词,但Hy @code{List}在处理邮箱地址列表之类的东西时不会比Python @code{list}带来更多好处.
@smaller{(译注: 早期版本Hy的@code{List}派生自@code{list}, 
现在派生自@code{tuple})}

model的默认表示形式(@code{hy.repr})使用单引号以提升可读性, 
所以@code{(hy.models.Integer 5)}被表示为@code{'5}. 
Python表示形式(@code{repr})使用构造器, 并且是默认美化打印的; 
你可以设置@code{hy.models.PRETTY}为@code{False}以全局禁用, 
或用@code{hy.models.pretty}上下文管理器临时配置.

@pyblock{
class hy.models.Object:
    Hy model的抽象基类, 表示form.

class hy.models.Lazy(gen):
    hy.read-many的输出. 它表示一系列form, 可作为迭代器. 对于读取宏来说, 
    仅在求值完前一个form后惰性地读取form是必要的; 见hy.read-many.
}

@subsection{非Form语法元素}

@subsubsection{Shebang}

如果Hy程序以#!开始, Hy假定它是Shebang并忽略它. 你的操作系统会负责处理它.

Shebang实际并非Hy的语法, 所以@code{hy.read-many}仅在选项@code{skip_shebang}被打开时才允许它.

@subsubsection{空白}

Hy的空白规则更为宽松, 这与Python以外的许多语言相似. 
空白可以分隔form(比如@code{a b}是两个form而@code{ab}是一个), 
可以出现在某些form内部(比如字符串字面量), 但在其他时候会被读取器忽略, 不产生model.

读取器只视ASCII空白符为空白, 即U+0009(水平制表符), U+000A(换行), 
U+000B(纵向制表符), U+000C(换页符), U+000D(回车)和U+0020(空格). 
非ASCII空白符, 如U+2009(宽空格), 被视为其他字符. 所以你可以在变量名中包含空格, 尽管这只会迷惑测试人员.

@subsubsection{注释}

注释以分号(;)开始, 到行末为止.

没有C风格的@code{/* ... */}风格多行注释, 但你可以用废弃前缀和字符串字面量达到类似效果.

@subsubsection{废弃前缀}

与Clojure类似, Hy支持可拓展数据标记(Extensible Data Notation)废弃前缀#_作为一种结构性注释. 当读取器遇到#_时, 它读取并废弃接下来的form. 所以#_与;类似, 只不过读取宏仍然生效, 
且常规解析在下一个form而非下一行恢复: @pycode{[dilly #_ and krunk]}与@code{[dilly krunk]}等价, 而@pycode{[dilly ; and krunk]}与
@pycode{[dilly}等价. 
;注释可嵌套于被#_废弃的form, 但#_在;注释中没有特殊含义.

@subsection{标识符}

标识符是Hy语法中很大的一类, 不止包括变量名, 还包括任何非空且不包含ASCII空白符或()[]{}"'`~的字符序列. 读取器会尝试将标识符读取为数字字面量; 如果失败, 读取为关键字; 如果失败, 
读取为点标识符; 如果都失败了, 读取为符号.

@subsubsection{数字字面量}

所有Python数字字面量语法在Hy中都是支持的, 产生@code{Integer}, @code{Float}或@code{Complex}. Hy还提供了一些扩展:

@itemlist[
  @item{逗号(,)和下划线(_)都可用于分隔数字, 所以@code{10_000_000_000}可写作@code{10,000,000,000}.}
  @item{整数可有前导零, 即使没有@code{0x}之类的前缀. 前导零不会像在C里一样把数字变为八进制. 要创建八进制, 使用Python的前缀@code{0o}.}
  @item{@code{Nan}, @code{Inf}和@code{-Inf}被理解为字面量, 产生@code{Float}.}
  @item{Hy允许被@code{complex}构造器理解的复数字面量, 如@code{5+4j}(这在Python中也是合法的, 但Hy将其读作单独的@code{Complex}, 在别处不支持中缀语法.)}
]

@pyblock{
class hy.models.Integer(number, *args, **kwargs):
    表示整型字面量(int).

class hy.models.Float(num, *args, **kwargs):
    表示浮点实数字面量(float).

class hy.models.Complex(real, imag=0, *args, **kwargs):
    表示浮点实数字面量(complex).
}

@subsection{关键字}

以冒号(:)开头的标识符, 比如@code{:foo}, 是关键字.

字面关键字最常用于在非宏调用中的特殊作用: 它设置关键字参数, 使之不按位置传参. 比如, @code{(f :foo 3)}以设置为@code{3}的参数@code{foo}调用函数@code{f}. 关键字在编译时被转义. 
为了防止字面关键字在表达式中被特殊对待, 你可以quote它, 或把它自身用作一个关键字参数, 如@code{(f :foo :bar)}.

除此以外, 关键字就是自运算的简单model对象. 其他Lisp的使用者应注意一般用字符串而非关键字是更好的选择, 因为Python的其他部分在Lisp使用关键字的地方使用字符串. 特别地, 
字符串比关键字更适合在字典中使用. 注意@code{(dict :a 1 :b 2)}与@code{{"a" 1  "b" 2}}等价, 与@code{{:a 1  :b 2}}不同(见字典字面量).

空关键字@code{:}在语法上合法, 但你无法用一个空关键字编译函数调用.

@pyblock{
class hy.models.Keyword(value, from_parser=False):
    表示一个关键字, 如:foo.

    属性:
        name - 关键字参数的字符串内容, 不包括前缀:, 不进行转义.

    __bool__()
        空关键字:为假, 否则为真.

    __call__(data, default=<object object>)
        获取data中名为(hy.mangle self.name)的内容. 所以, (:foo bar)与(get bar "foo")等价(与(get bar :foo)不同; 
        键总是字符串, 而非hy.models.Keyword).

        第二个可选参数是默认值. 如果提供了, get引发的KeyError会被捕获, 返回默认值.
}

@subsection{点标识符}

点标识符因其中的字符.而得名. 它没有自己的model, 因其实际是表达式的语法糖, @code{foo.bar.baz}这样的语法与@pycode{(. foo bar baz)}等价. 一般的规则是, 
点标识符长得像单个.分隔的两个或多个符号(自身不含.), 结果是一个以@pycode{.}为首个元素, 各个符号为余下元素的表达式.

点标识符也可始于一个或多个., 如@code{.foo.bar}或@code{..foo.bar}, 结果表达式有相同开头(.或..之类的), 以@code{None}为下一个元素. 所以@code{..foo.bar}与@code{.. None foo bar}等价. 
在有前导.的情况下, 你可以只用一个符号, 所以@code{.foo}是一个合法标识符, 与@pycode{(. None foo)}等价.

见@hyperlink["#" "点宏"]以了解编译结果. 也可以了解以以点开头的点标识符开头的表达式的特殊行为. 注意Hy默认提供了@pycode{.}和@code{...}的定义, 但没有@code{..}, @code{....},
@code{.....}等. 所以@code{..foo.bar}在特殊对待之的宏(如@code{import})的宏以外没有任何用处.

@subsection{符号}

符号(Symbol)包含了所有的标识符. 在多数情况下, 符号在转义后被编译为Python变量名. 你可以用@code{quote}运算符或调用@code{Symbol}构造器(所以, 
@code{Symbol}扮演与其他Lisp中@code{intern}函数类似的角色)创建符号实例. 例子包括@code{hello}, @code{+++}, @code{3fiddy}, @code{$40}, @code{just✈wrong}和@code{🦑}.

点只在符号中的每一个字符都是点时才合法. 所以, @code{a..b}和@code{a.}既非点标识符亦非符号; 它们只是语法错误.

特别地, 符号@code{...}编译至和Python一样的@code{Ellipsis}对象.

@pyblock{
class hy.models.Symbol(s, from_parser=False):
    表示符号.

    符号对象在get, len()和bool等操作中表现得像字符串; 特别地, (bool (hy.models.Symbol "False"))返回真. 使用hy.eval以求值一个符号.
}

@subsubsection{转义}

因为Hy的符号与关键字规则比Python的更宽松, Hy使用转义算法以将其名字转化到Python合法的名字. 步骤如下:

@itemlist[#:style 'ordered
  @item{移除前导下划线. 下划线一般是ASCII下划线@code{_}, 但也可以是任何被标准化(根据NFKC)为@code{_}的Unicode字符. 前导下划线在Python中有特殊意义, 
  并且Python在测试前会标准化Unicode字符, 所以Hy会先处理剩下的名字, 再把前导下划线加回去.}
  @item{将ASCII连字符(@code{-})转化为下划线(@code{_}). 所以, @code{foo-bar}变为@code{foo_bar}. 如果名字以一个连字符开始, 第一个连字符不被转化, 这样就不会产生多余的前导下划线了. 
  所以@code{--has-dashes?}在这一步变为@code{_has_dashed?}.}
  @item{如果以ASCII@code{?}结束, 移除之并在前端添加@code{is_}. 所以, @code{tasty?}变为@code{is_tasty}, 而@code{-_has_dashed?}变为@code{is_-_has_dashes}.}
  @item{如果名字仍然Python不合法, 做以下的转换. 名字可能因其包含在某个位置永远Python不合法的字符而不合法. 
  
  1. 在前端添加@code{hyx_} 
  
  2. 将每个非法字符替换为@code{XfooX}, 其中@code{foo}是其小写Unicode字符名, 以下划线替代空格, @code{H}替代连字符. 移除前导连字符和@code{X}. 如果字符没有名字, 
  用@code{U}加其小写十六进制码.所以, @code{green☘}变为@code{hyx_greenXshamrockX}, 而@code{is_-_has_dashes}变为@code{hyx_is_XhyphenHminusX_has_dashes}.}
  @item{将第一步中移除的前导下划线翻译为ASCII形式再加回去. 所以, @code{(hy.mangle '_tasty?)}是"@code{_is_tasty}"而非"@code{is__tasty}", 而@code{(hy.mangle '__-_has-dashes?)}
  是"@code{__hyx_is_XhyphenHminusX_has_dashes}".}
  @item{最后, 将剩下的非ASCII字符规范化. 结果仍可能是非ASCII的(如@code{α}已是合法且规范的Python名字了, 所以它在转义中没有改变), 但现在符号仅当其指向代表Python时才是相等的了.}
]

你可以用@code{hy.mangle}函数调用转义, 或尝试(虽然可能失败)用@code{hy.unmangle}逆转这个过程.

转义不是值得你关心的东西, 但你可能会在错误信息和@code{hy2py}的输出等中见到转义过的名字. 应当注意的是, 转义, 以及相反的@code{hy.unmangle}, 不是一对一的. 
两个不同的标识符可能被转义为相同的字符串, 并被编译为相同Python变量名. 主要的后果是(非起始的)@code{-}和@code{_}是可互换的, 所以你不能以@code{foo-bar}和@code{foo_bar}为不同的变量.

@subsubsection{字符串字面量}

Hy允许双引号字符串(如@code{"hello"}), 但不允许Python一样的单引号字符串. 单引号字符'被保留用来防止求值一个form(如@code{'(+ 1 1)}), 与大多数Lisp一样(见@hyperlink["#" "附加语法糖"]). 
Python的所谓三引号字符串(如@pycode{'''hello'''}和@code{"""hello"""})不被支持. 然而, 与Python不同, 字符串字面量本身就可包含换行; 而且, Hy提供了方括号字符串. 
为了与Python的三引号字符串一致, 字面量中的换行被读作@code{"\n"}(U+000A, 换行), 不管其在实际代码中如何.

字符串字面量支持一系列反斜杠转义. 不被识别的转义序列是语法错误@smaller{(译注: 在Python中不是)}. 用@code{r}前缀, 如@code{r"slash\not"}, 创建字面解释反斜杠的"原始字符串".

与Python类似, Hy默认所有字符串字面量为Unicode字符序列. 结果是model类型@code{String}. 你可以用前缀@code{b}以将其作为字节序列, 产生@code{Bytes}.

与Python不同, Hy只允许小写字符串前缀@code{r}, @code{b}和@code{f}, 不允许没有实际作用的前缀@code{u}.

F-字符串是类似字符串的复合类型, 将在下文阐述.

@pyblock{
class hy.models.String(s=None, brackets=None):
    表示字符串字面量(str).

    属性:
        brackets - 方括号字符串使用的解析到实例的自定义分隔符, 不是方括号字符串的话为None. 输出的结果不包含方括号和#, 所以字面量#[[hello]]的brackets属性是空字符串.

class hy.modelsBytes:
    表示一个字节串字面量(bytes).
}

@subsubsection{方括号字符串}

Hy支持一种被称为"方括号字符串"的字符串字面量替代形式, 与Lua的方括号语法类似. 方括号字符串有自定义分隔符, 与其他语言中的Here-Document@smaller{(译注: Ruby, 
Shell等语言中插入大块字符串的语法)}相似. 方括号字符串以@pycode{#[FOO[}开始, 到@pycode{#]FOO]}为止, 其中@code{FOO}是任何不包含@pycode{[}或@pycode{]}的字符串, 包括空串. 
(如果@code{FOO}为@code{f}或始于@code{f-}, 方括号字符串或成为F-字符串). 例如:

@pyblock{
=> (print #[["That's very kind of yuo [sic]" Tom wrote back.]])
"That's very kind of yuo [sic]" Tom wrote back.
=> (print #[==[1 + 1 = 2]==])
1 + 1 = 2
}

方括号字符串总是原始Unicode字符串, 不允许@code{r}或@code{b}前缀.

方括号字符串可包含换行, 但如果以换行开始, 它会被移除, 所有你可以从左分隔符下面的一行开始方括号字符串, 对内容没有影响. 第一个以后的任何前导换行都被保留.

@subsubsection{序列Form}

序列form(@code{Sequence})是以定义好的顺序包含任意数量其他form的嵌套form.

@pyblock{
class hy.models.Squence(iterable=(), /):
    所有类序列form的抽象基类. 序列model可以像tuple一样操作: 你可以迭代, 索引, 或用+拼接, 但你不能添加, 移除或替换元素. 将一个序列拼接至另一个可迭代对象重用了其左手边对象, 
    这在你想将model拼接制作宏时时有用的.

    当你在递归下降遍历model树时, 可以用(isinstance x hy.models.Sequence)测试是否要迭代x. 你还可以用Hyrule函数coll?做这件事.
}

@subsubsection{表达式}

表达式(@code{Expression})被括号标注: @code{( ... )}. 编译器通过其第一个元素求值表达式.

@itemlist[
  @item{如果是一个符号, 且符号是一个定义了的宏, 调用这个宏.
  
  例外: 如果这个符号也是@code{hy.pyops}里的一个函数, 且一个参数是@code{unpack-iterable} form, 调用@code{pyops}函数而非宏. 这使那些看似合理的表达式正确指向, 否则就会失败. 
  例如, @pycode{(+ #* summands)}被理解为@pycode{(hy.pyops.+ #* summands)}, 因为Python不提供用加法表达式求和长度未知的列表的方法.}
  @item{如果它是一个类似于@pycode{(. None ...)}(通常由@code{.add}这样的点标识符产生), 它有助于构造一个以@code{None}后的元素为实例的方法调用: 所以, @code{(.add my-set 5)}与
  @pycode{((. my-set add) 5)}等价, 在Python中为@code{my_set.add(5)}}
  @item{否则, 表达式被编译为Python调用, 以第一个元素为调用者. (所以你可以写@code{((do setv) ...)}以调用和宏同名的函数)剩下的form被理解为参数. 使用@code{unpack-iterable}或
  @code{unpack-mapping}以将数据结构在运行时解开为单独参数.}
]

空表达式@code{()}在读取器层面是合法的, 但它没有任何意义. 编译它产生一个错误. 用@code{#()}创建空元组.

@pyblock{
class hy.models.Expression(iterable=(), /):
    表示括号包围的Hy表达式.
}

@subsubsection{列表, 元组与集合字面量}

字面量列表(@code{List}), 元组(@code{Tuple})和集合(@code{Set})依次用@code{[ ... ]}, @code{#( .. )}和@code{#{ .. }}标记.

@pyblock{
class hy.models.List(iterable=(), /):
    表示字面列表.

    许多宏特殊对待这个model类型, 不将其用来定义列表. 比如, defn期望其参数表为方括号分隔的列表, 而for期望迭代子句的列表.

class hy.models.Tuple(iterable=(), /):
    表示字面元组.

class hy.models.Set(iterable=(), /):
    表示字面集合. 与实际的集合不同, 这个model保持了重复项和元素顺序.
}

@subsubsection{字典字面量}

字典字面量(@code{dict}, @code{Dict})被@code{{ ...}}标记. 偶数子form(从零开始)为键, 奇数子form为值. 例如, @code{{"a" 1  "b" 2}}产生映射@code{"a"}到@code{1}, 
@code{"b"}到@code{2}的字典. 编译有奇数子form的字典产生错误.

和Python一样, 用关键字参数调用@code{dict}通常比字面量字典更方便.

@pyblock{
class hy.models.Dict(iterable=(), /):
    表示字面字典. 提供了keys, values和items方法, 分别返回一个列表, 尽管这个model类型与正常dict类型没有任何关系. 有奇数个子form时, keys返回最后一个孩子, 而values和items会忽略它.
}

@subsubsection{格式化字符串}

格式化字符串(又叫"F-字符串"或"格式化的字符串字面量")时嵌入了代码(可能还有格式化指令)的字符串字面量. 结果是@code{FString}. Hy的F-字符串和Python的F-字符串十分相似, 
只不过嵌入的代码是Hy而非Python.

@codeblock{
=> (print f"The sum is {(+ 1 1)}.")
The sum is 2.
}

因为@code{=}, @code{!}和@code{:}是Hy中的标识符符号, Hy用解析正好一个for的方法决定替换区域的终止(以及调试@code{=}和格式控制符的开始). 你可以用@code{do}组合多个form. 
空白可能在终止form中是必需的:

@codeblock{
=> (setv foo "a")
=> (print f"{foo:x<5}")
…
NameError: name 'hyx_fooXcolonXxXlessHthan_signX5' is not defined
=> (print f"{foo :x<5}")
axxxx
}

与Python不同, 空白在格式控制符间是允许的.

也与Python不同的是, 注释和反斜杠在替换区域中是允许的. 解析其中form的读取器和解析语言其余部分的是相同的. 所以@code{f"{"a"}"}是合法的, 与@code{"a"}等价.

@pyblock{
class hy.models.FString(s=None brackets=None):
    用一个hy.models.String和hy.models.FComponent的可迭代集合表示格式化字符串. 设计来自ast.JoinedStr.

    属性:
        brackets - 与hy.models.String里的相同.

class hy.models.FComponent(s=None, conversion=None):
    与ast.FormattedValue类似. 第一个节点包含被格式化的值的序列. 剩下的包含节点的格式化选项(如果有的话).
}

@subsubsection{附加语法糖}

语法糖在用特定宏构造两项的表达式时时可用的. 当读取器遇到语法糖字符时, 包含对应宏的表达式被创建, 不需要括号. 所以, 单个'是@code{quote}的简写, 
@code{'FORM}被读作@code{(quote FORM)}. 空白是被允许的, 比如@code{' FORM}. 这些都在读取器层面被处理, 所以产生的model用或不用语法糖都一样.

@table[
  '("宏" "语法")
  '("quote" "'FORM")
  '("quasiquote" "`FORM")
  '("unquote" "~FORM")
  '("unquote-splice" "~@FORM")
  '("unpack-iterable" "#* FORM")
  '("unpack-mapping" "#** FORM") 
]

@subsection{读取宏}

跟着一个符号的井号#调用名为这个符号的读取宏. (调用未定义读取宏产生语法错误)返回之前, 读取宏掌握剩余代码的控制权.

@section{语义}

本章描述了Hy语义中在别处(如@hyperlink["#" "宏"])中没有介绍的与Python的不同.

@subsection{隐式名字}

所有编译单元(基本上是模块)都隐式地以@code{(import hy)}开始. 你可以在@code{hy2py}的输出中看见它. 
这样做的目的是确保hy编译代码时能检索到任何它所需要的名字. 例如, @code{(print '(+ 1 1))}需要构造一个@code{hy.models.Expression}. 
所以你在给名字@code{hy}赋值, 即使是局部赋值时, 也要小心, 因为生成的代码在尝试访问@code{hy}模块时可能会发生错误. 好处是, 你可以写
@code{(print (hy.repr #(1 2)))}这样的代码而无需先显式导入@code{hy}.

如果你限制地只使用Hy的一个子集, 你或许可以写一个@code{hy}程序, 用@code{hy2py}把它翻译到Python, 去掉@code{import hy}, 
然后获得一个不依赖Hy本身的程序. 不幸的是, 对于Hy编译器来说, Python过于动态, 以至于它不能预知上述情况是否成立. 这也是为什么自动导入是不可选择的的原因.

Hy需要在实现一些技巧时创建临时变量. 例如, 为了在Python中表示@code{(print (with ...))}, @code{with}的结果会被设置到一个临时变量中. 
这些名字以@code{_hy_}开头, 所以应避免在你自己的变量名中使用这个前缀. 这些临时变量的作用域和普通变量是一样的, 不会被显示回收. 所以理论上, 
它们会浪费内存, 并使@code{object.__del__()}的调用晚于你的预料. 如有疑问, 查看@code{hy2py}的输出.

@subsection{求值顺序}

和Python以外的许多语言@smaller{(译注: 比如Haskell等纯函数式语言)}类似, Hy不在所有情况下都确保函数参数求值的顺序. 准确地说, 
@code{hy.models.Sequence}的子model的求值顺序是不确定的. 例如, @code{(f (g) (h))}可能会在@code{(g)}前求值@code{(h)}(的一部分), 
尤其是当@code{f}是一个函数, 而@code{h}是一个产生Python语句的宏的时候. 所有如果你想确保@code{g}先调用, 在@code{f}之前调用它.

@subsection{字节码何时重新生成}

Hy第一次运行一个文件时, 它会产生一个字节码文件(除非环境变量@code{PYTHONDONTWRITEBYTECODE}被设置了). 然后, 如果源码文件没有改变, 
Hy会载入字节码而非重新编译源码. Python的行为类似, 但重新编译和载入字节码之间的差异在Hy中更大, 因为Hy允许你用宏, 读取宏, 
@code{eval-and-compile}等方式在编译期运行和生成代码. 你可能会对下面的行为感到诧异:

@pyblock{
$ echo '(defmacro m [] 1)' >a.hy
$ echo '(require a) (print (a.m))' >b.hy
$ hy b.hy
1
$ echo '(defmacro m [] 2)' >a.hy
$ hy b.hy
1
}

为什么第二次调用@code{b.hy}时不打印@code{2}呢? 因为@code{b.hy}没有变化, 所以它不被重新编译, 字节码仍然使用宏@code{m}的前一次扩展.

@section{宏}

@subsection{使用gensym来编写更安全的宏}


编写宏时, 必须小心避免捕获外部变量或使用可能与用户代码冲突的变量名.

我们会使用一个样例@code{nif}(完整介绍见@hyperlink["#" "http://letoverlambda.com/index.cl/guest/chap3.html#sec_5"].)
@code{nif}类似于数字版的@code{nif}, 包含一个数字表达式和三个分别在表达式值为正, 零或负时被调用的form.

第一版应该类似这样:

@codeblock{
(defmacro nif [expr pos-form zero-form neg-form]
  `(do
    (setv obscure-name ~expr)
    (cond (> obscure-name 0) ~pos-form
          (= obscure-name 0) ~zero-form
          (< obscure-name 0) ~neg-form)))
}

其中@code{obscure-name}用于避免使用和其他代码冲突的变量名. 然而, 尽管出于好心, 它的结果没有绝对的保证.

方法@code{gensym}用来生成一个独一无二的符号以应对这种情况. 更好的@code{nif}版本会是:

@codeblock{
(defmacro nif [expr pos-form zero-form neg-form]
  (setv g (hy.gensym))
  `(do
    (setv ~g ~expr)
    (cond (> ~g 0) ~pos-form
          (= ~g 0) ~zero-form
          (< ~g 0) ~neg-form)))
}

这是只有一个符号的简单情况. 但如果需要多次调用gensym的话, 有一个叫@code{with-gensyms}的宏. 
它会扩展为@code{setv} form:

@codeblock{
(with-gensyms [a b c]
  ...)
}

扩展为:

@codeblock{
(do
  (setv a (hy.gensym)
        b (hy.gensym)
        c (hy.gensym))
  ...)
}

所以我们可以这么重写@code{nif}:

@codeblock{
(defmacro nif [expr pos-form zero-form neg-form]
  (with-gensyms [g]
    `(do
      (setv ~g ~expr)
      (cond (> ~g 0) ~pos-form
            (= ~g 0) ~zero-form
            (< ~g 0) ~neg-form))))
}

我们还可以进一步写一个做这种事的宏. @code{defmacro/g!}会自动将以@code{g!}开头的符号转化为对@code{gensym}的调用. 
如此@code{g!a}会变成@code{(hy.gensym "a")}.

使用@code{defmacro/g!}的最终版会是:

@codeblock{
(defmacro/g! nif [expr pos-form zero-form neg-form]
  `(do
    (setv ~g!res ~expr)
    (cond (> ~g!res 0) ~pos-form
          (= ~g!res 0) ~zero-form
          (< ~g!res 0) ~neg-form)))
}

@smaller{(译注: 上述宏在新版Hy中均被移至hyrule)}

@section{Hy REPL}

Hy的读取-求值-打印循环(REPL)以@code{hy.REPL}实现. REPL可以从命令行互动打开, 或者在程序中用实例方法@code{hy.REPL.run}打开

有两个REPL使用的环境变量: 指明REPL输入历史位置的@code{HY_HISTORY}, 和指明REPL启动时运行的文件的@code{HYSTARTUP}

@pyblock{
class hy.REPL(spy=False, output_fn=None, locals=None, filename='<stdin>', allow_incomplete=True):
    Hy中code.InterativeConsole的子类.

    将下面的代码插入你的代码中对可以进行方便的互动调试:

    (.run (hy.REPL :locals (locals)))
    注意, 使用code.interact()时, 在REPL内对(locals)的修改不会影响外部作用域.

    run()[source]:
        启动REPL. 结束时返回0.
}

@subsection{输出函数}

默认情况下, 各个REPL输入的返回值是用@code{hy.repr}打印的. 你可以通过用命令行参数@code{--repl-output-fn}设置打印函数. 
用@code{repr}得到和Python REPL一样的表示形式.

HY和Python一样, 不管输出函数为何, 当值是@code{None}时不会输出.

@subsection{特殊变量}

REPL包含了一些特殊变量. @code{*1}保存了最近的输入结果, 相当于Python REPL里的@code{_}. 
@code{*2}保存了在这之前的结果, @code{*3}保存了再之前的, 以此类推. @code{*e}保存了最近的未捕获异常.

@subsection{初始化文件}

在REPL初始化文件里定义的宏或Python对象会被代入Hy REPL的作用域. 两个变量在初始化文件中有特殊意义.

@codeblock{
repl-spy
  如果为真, 在执行每段Hy代码前打印Python等价.
repl-output-fn
  输出函数, 须为单元可调用对象.
}

Hy初始化文件可以做很多事, 包括设置横幅信息或改变输入提示. 下面展示了一些可能性:

@codeblock{
  ;; 用eval-and-compile包裹它们, 使这些Python包在此文件中定义的宏里也是可用的.
  (eval-and-compile
    (import sys os)
    (sys.path.append "~/<path-to-global-libs>"))

  (import
    re
    json
    pathlib [Path]
    hy.pypos *
    hyrule [pp pformat])

  (require
    hyrule [unless])

  (setv
    repl-spy True
    repl-output-fn pformat
    ;; 将REPL提示=>设为绿色.
    sys.ps1 "\x01\x1b[0;32m\x02=> \x01\x1b[0m\x02"
    ;; 将REPL提示...设为红色.
    sys.ps2 "\x01\x1b[0;31m\x02... \x01\x1b[0m\x02")

  (defn slurp [path]
    (setv path (Path path))
    (when (path.exists)
      (path.read-text)))

  (defmacro greet [person]
    `(print ~person))
}

@section{环境变量}

Hy特殊对待一些环境变量. 布尔变量视空字符串为假, 其他任何情况都为真.

@pyblock{
HYSTARTUP

   (默认: 无)包含在启动REPL时执行的Hy源码的文件路径.

HY_DEBUG

   (默认: 假)和HY_FILTER_INTERNAL_ERRORS差不多.

HY_FILTER_INTERNAL_ERRORS

   (默认: 真)是否在栈回溯中隐藏指向Hy内部的异常. 它们通常对Hy程序员没有帮助.

HY_HISTORY

   (默认: ~/.hy-history)保存REPL输入历史的文件路径.

HY_MESSAGE_WHEN_COMPILING

   (默认: 假)是否在编译每个Hy源码文件时向标准输入流中打印"Compiling FILENAME". 
   它可用于判断文件是被载入自字节码还是被重新编译.
}

@section{命令行接口}

Hy提供了很多帮助编写Hy代码的命令行程序.

@subsection{hy}

@code{hy}是Hy中对标CPython中@code{python}的命令行接口. 比如, 不用参数启动@code{hy}会打开hy REPL,
而@code{hy foo.hy a b}以@code{a}和@code{b}为命令行参数运行Hy程序@code{foo.hy}. 
见@code{hy --help}获取完整选项列表, 和@hyperlink["#" "Python文档"]的更多细节. 
下面是一些Hy特有的细节.

@codeblock{
-m <module>
   类似Python的-m, 但模块名会被转义.

--spy
    在运行每段Hy代码前打印Python等价:

    => (+ 1 2)
    1 + 2
    3

--repl-output-fn
    见输出函数. 它可以为一个Python内建对象的名字(多半是repr), 或者是一个有点的名字, 如foo.bar.baz.
    在第二种情况下, Hy会试图这样(import foo.bar [baz])来导入对应的对象.
}

@subsection{hy2py}

@code{hy2py}是一个将Hy源码转换为Python源码的程序. 用@code{hy2py --help}了解用法. 它的输入可以来自标准输入, 文件或文件夹. 
如果参数是文件夹, 输出参数(@code{--output}/@code{-o})必须被指定. 如果这个参数被指定, 输出会被写入一个文件夹或文件, 否则打印在标准输出.

@margin-note{
@code{hy2py}可以执行任意代码. 不要提供不被信任的输入.
}

@subsection{hyc}

@code{hyc}是一个Hy源码编译为Python字节码的程序. 用@code{hyc --help}了解用法. 
生成的字节码文件根据Python可执行文件的一般策略命名和放置, 即依照@code{importlib.util.cache_from_source}.

@margin-note{
@code{hyc}可以执行任意代码. 不要提供不被信任的输入.
}

@section{Python交互}

虽然作为Lisp, 但Hy力求与Python完全兼容. 这意味着任何Python模块或包都可以被Hy导入, 反之亦然.

:ref:`Mangling <mangling>` allows variable names to be spelled differently in
Hy and Python. For example, Python's ``str.format_map`` can be written
``str.format-map`` in Hy, and a Hy function named ``valid?`` would be called
``is_valid`` in Python. You can call :hy:func:`hy.mangle` and
:hy:func:`hy.unmangle` from either language.
名字转义使得变量名在Hy和Python中有不同的拼写. 比如, Python的@code{str.format_map}可在Hy中写作@code{str.format-map}, 
而名为@code{valid?}的Hy函数可在Python中以@code{is_valid}调用. 两个语言都可以调用@code{hy.mangle}和@code{hy.unmagle}.

@code{在Hy里使用Python}

在Hy里使用Python模块时, 仅需@code{import}它. 无需其他任何东西.

你可以用宏@code{py}和@code{pys}将Python代码直接嵌入Hy程序, 
或用@code{eval}, @code{exec}等标准Python工具执行或操作字符串形式的Python代码.

@subsection{在Python中使用Hy}

只要提前导入了Hy, 就可以只用@code{import}来使用在Python中导入Hy模块. 
导入@code{hy}是为了创建导入Hy模块的钩子. 注意, 你可以在一个包装器文件
(比如一个包的@code{__init__.py})中进行@code{import hy}; 这对于已发布的包是有用的. 

无法将宏或读取宏导入Python模块, 因为根本不能在Python中调用它们.

@bold{Under Construction}
