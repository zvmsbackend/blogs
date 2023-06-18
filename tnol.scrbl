#lang scribble/manual

@(require "util.rkt")

@title{The Nature of Lisp(Translated)}

@smaller{Monday, May 8, 2006}

@section{导言}

第一次在互联网的角落里撞入Lisp社区时我已是一个经验丰富的程序员了. 当时我觉得自己已经学过了许多编程语言. 
我很自豪地拥有C++, Java, C#等的记录, 并且认为我知道关于编程语言的所有知识. 实际上, 我大错特错.

我对Lisp的第一次尝试在第一眼看到样例代码时就破灭了. 我想我当时的想法也出现在无数经历相似的人的脑海中: 
"他妈的谁愿意用有这种傻逼语法的垃圾语言?!"我可不愿学一门其创造者不愿设计一种漂亮语法的语言. 
总之, 我快被Lisp的著名括号弄瞎了!

眼睛缓过来之后, 我向几个Lisp教徒倾诉了我的困惑. 几乎在一瞬间我被一套标准的回复淹没了: 
Lisp的括号只是表面的问题, Lisp中代码与数据的同像表示带来很多好处(这当然是对XML的巨大改进), 
Lisp有强得离谱的元编程能力, 可以让程序自我编程, Lisp允许创造针对特定问题的迷你语言, 
Lisp模糊了运行时和编译时的界限, Lisp, Lisp, Lisp...这份回复是相当可观的. 不消说没一个我能看懂. 
没有人能用具体例子把这些特性的好处说明白, 因为这些技巧被认为只适用于大型软件系统. 
和他们争论了很久关于普通语言能否完成这些任务后, 我放弃了. 我不想花几个月学习一种有吓人语法的语言以理解没有实用例子的抽象特性. 
我的时机未到.

几个月以来Lisp信徒们一直在施压. 我被他们说倒了. 许多我认识且尊敬的非常聪明的人都用一种近乎宗教的虔诚赞美Lisp. 
它肯定有什么秘密, 我不得不上手实践! 最终对知识的渴望拿下了我. 我打定主意, 咬紧牙关, 
然后陷入了几个月的苦思冥想. 这是一场穿越无尽的未知之海的旅程. 我把脑子挖了出来, 
洗了一遍, 然后再放回去. 我穿越七层地狱然后返回. 最后我顿悟了.

启迪在瞬间到来. 某一刻我还一无所知, 下一秒所有知识都再我的面前展开. 我完成了涅槃. 
无数次我听到过人们对Eric Raymond的名言的引用: "Lisp值得你为深刻的启示而学习. 
这份启示会让你以后成为更好的程序员, 即使你不使用Lisp本身. "我从未理解它. 我从不相信它. 
而最后, 经历了无数苦痛之后, 它变得可以理解了. 它所蕴涵的真理超出我的想象. 
我的意识处于一种神圣的状态, 一种瞬间改变了我对计算机科学的认识的启迪.

在那一秒内我成为了Lisp教派的一员. 我感到一种忍者大师经历过的感觉: 我必须在余生将习得的知识传播给十个灵魂. 
我走上了这条道路. 我重述了多年来被灌输的论点(此时我才看得懂它们! ), 希望能转化没有戒心的旁观者. 
它没有用. 我的坚持激发了一些人的兴趣, 但他们的好奇心一见到样例Lisp代码就消融了. 
也许经年的宣传能带来一些新Lisper, 但我不满意. 一定有更好的方法.

我仔细地考虑了这个问题. Lisp在本质上有什么难点, 能阻止最聪明, 最熟练的程序员理解它吗? 
没有. 不管怎样, 我已经学会了, 而既然我学会了, 任何人都可以. 所有是什么令Lisp如此难以理解呢? 
答案一如既往地出人意料. 当然! 传授任何人新概念时都要基于它们已理解的概念! 
如果过程有趣, 且事情被正确地解释, 新概念就会和辅助他们理解的旧概念一样直观. 
这就是问题所在! 元编程, 代码和数据的同像性, 自我修改的程序, 领域特定迷你语言, 
对这些概念的解释都无法与熟悉的领域关联起来. 我怎么能指望有人理解它们呢? 难怪人们想要具体的例子了. 
当时我的宣传听上去肯定像火星话一样.

我和几个Lisper分享了这个想法. "啊, 这些概念当然不能用熟悉领域中的术语解释, "他们说, 
"它们是如此与众不同, 它们不像人们学过的任何东西." 这是一个无力的推辞. "我不认为这样. "
我说. 答复是一致的: "为什么不试一试呢?" 所以我试了. 这篇文章就是我尝试的成果. 它会用熟悉, 
直观的概念阐释Lisp. 我恳请勇敢者读下去. 拿上你最喜欢的饮料. 深吸一口气. 
准备把它呼出去. 哦, 还有, 愿原力与你同在.

@section{重返XML}

千里之行始于足下. 通向真理之旅也不例外, 而我们的第一步正好是XML. 关于XML还有什么可说的吗? 
实际上, 还真有一些. 虽然XML本身没有什么有意思的了, 但它与Lisp的关系却是惊人的. 
XML有Lisp教徒需要的所有熟悉概念. 它是连接普通程序员和Lisp的桥梁. 让我们骑上老马, 收拾行装, 
踏上先人未曾冒险过的XML荒原. 是时候看到月亮的另一面了.

表面上XML就是一套用来以人类可读形式表达任意层级数据的标准语法. TODO列表, 网络页面, 
医疗记录, 保险索赔, 配置文件, 这些都可以用XML表述. 让我们以一个简单的TODO列表为例
(几节后你会以一种全新的眼光看待它):

@pyblock{
<todo name="housework">
    <item priority="high">Clean the house.</item>
    <item priority="medium">Wash the dishes.</item>
    <item priority="medium">Buy more soap.</item>
</todo>
}

我们将自己喜欢的XML解析器作用在这份TODO列表时会发生什么呢? 被解析后, 它在内存中是如何表示的呢? 
最自然的表示法, 当然, 就是树 - 层级数据的完美结构. 说到底, XML不过是一棵被表示为人类可读形式的树. 
任何可以被表示为树的数据都可以被表示为XML, 反之亦然. 我希望你已经理解了这一点. 这对下面的概念是相当重要的.

让我们更进一步. 还有什么类型的数据经常被表示为树? 这里可能性似乎有点多, 所以我来给出一些提示 - 
试着回忆你上过的编译原理课. 如果你还记得源码解析后被储存在一棵树里, 那么你就走到正道上了. 
这不令人感到奇怪, 因为源码就是有层级结构的: 函数包含参数和代码块, 代码块包含表达式和语句, 
表达式包含变量和运算符, 如此种种.

让我们将任何树都可以被序列化为XML的结论应用到代码上. 如果任何代码都被最终表示为树, 
且任何树都可以被序列化为XML, 那么所有代码都可以被转化为XML, 对吗? 让我们用一个简单的例子解释这个有趣的点子. 
考虑下面的函数:

@pyblock{
int add(int arg1, int arg2)
{
    return arg1 + arg2;
}
}

你可以将这个函数定义转化到其XML等价吗? 事实上, 这十分简单. 有很多自然的方法可以做到这一点. 
下面是一种可能的XML结果:

@pyblock{
<define-function return-type="int" name="add">
    <arguments>
        <argument type="int">arg1</argument>
        <argument type="int">arg2</argument>
    </arguments>
    <body>
        <return>
            <add value1="arg1" value2="arg2" />
        </return>
    </body>
</define>
}

我们可以用任何语言做这个简单的练习. 我们可以将任意代码转化为XML, 然后将结果XML转化为源码. 
我们可以写一个转换器, 将Java转换到XML, 再写一个把XML转回Java. 我们可以对C++做一样的事情
(如果你怀疑是否真的有疯子愿意这么做的话, 看看@hyperlink["#" "GCC-XML"]). 
而且, 我们可以将源码以XML为中间语言将一种语言翻译到另一种, 如果它们特性相仿而语法不同的话
(这某种程度上在主流语言中是很常见的). 我们可以用Java2XML转换器将Java程序转换为XML, 
然后用XML2CPP把它转到C++代码. 运气好的话(如果我们避免使用一些Java有而C++没有的特性), 
我们会得到可用的C++程序. 挺酷的, 对吧!

这些都有力地证明了XML可以作为通用源码储存形式. 我们将可以创造出一类使用相同语法的语言, 
并编写转化器将现存代码翻译到XML. 如果我们真的实现了这个想法, 不同语言的编译器将不用特定语法的解析器 - 
它们只需要一个XML解析器把XML直接转化为抽象语法树. 

现在你大概在琢磨我为什么要讨论XML以及它跟Lisp有什么关系(毕竟Lisp比XML早了30年). 
我承诺很快一切就要变得清晰了. 在第二步之前, 让我们进行一个简单的思维练习. 
看一眼上面的"@code{add}"函数的XML版本. 你该如何定位它? 是数据, 还是代码? 考虑一下, 
你就会发现这段XML可以被放到两个类别里面. 它是XML, 仅仅是编码到标准格式的信息(这正是GCC-XML所做的). 
它在一个文件里面, 无法被执行. 它是数据. 但等一下! 本质上它就是相同的"@code{add}"函数的另一种书写形式, 
一旦被解析, 这颗树就可以被丢给一个编译器然后被我们执行. 我们可以为这个XML代码写一个简单的解释器直接执行它. 
另外, 我们也可以把它转化到Java或C++代码, 编译它, 然后运行. 它是代码.

所以我们到了哪一步? 看起来我们处于一个有意思的点. 一个本应难以理解的概念现已惊人地简单而直观! 
代码就是数据, 并且从来如此! 这是否意味着数据也总是代码? 这听起来很疯狂, 但可能是真的. 
我承诺你会以一种全新的眼光看待上面的TODO列表, 记得吗? 让我重申这个承诺. 但我们还不足以讨论这一点. 现在我们且继续前行.

稍早一些我提到过我们可以轻松地写一个执行@code{add}函数XML片段的解释器. 当然这只是理论上的. 
哪个脑子正常的人想要付诸实践呢? 唔, 确乎有人不同意这一点. 你可能已经在工作中见到并用过他们的成果了. 
你兴奋了吗? 如果是的话, 前进!

@section{重返Ant}

现在我们已经到了月球的暗面. 让我们先保持安静. 我们仍需探索更多, 所以迈出下一步吧. 
闭上眼, 想起2000年那个寒冷多雨的冬夜. 一个叫James Duncan Davidson[1]的杰出程序员正在编写他的Tomcat微服务容器. 
到了构建的时候他小心地保存了所有文件然后运行了@code{make}. 错误. 成吨的错误. 哪里出问题了. 
细心检查后James惊呼: "是tab前面多余的空格让我的命令跑不了吗?!"的确, 问题就出在这里. 又一次! 
James受够了. 云层后的月光鼓舞了他. 他创建了一个Java项目, 快速地写了一个简单但出人意料地有用的工具. 
这个天才的灵感使用Java属性文件来获取项目构建信息. James可以用漂亮的格式写出makefile而不必担心天杀的空格了. 
这个工具通过解释属性文件并采取适当行为来完成困难的构建. 这很酷. 又一个很酷的工具. Ant.

用Ant构建了几个月的Tomcat后Java属性文件被证明不足以表述复杂的构建指令. 文件要被签出, 
复制, 编译, 发送到别的机器, 然后被单元测试. 失败了, 邮件要被发送给正确的人. 成功了, 
"Bad to the Bone"的音乐要以最大音量被外放. 结束后音量要被设回原来的. 是的, Java属性文件不够用了. 
James需要一个更灵活的解决方案. 他不想写自己的解析器(尤其因为他想要有工业标准的方案). 
XML似乎是一个合理的方案. 几天内Ant被移植到了XML. 它是面包发明以来最伟大的突破.

所以Ant是怎么工作的? 很简单. 它使用有特定指令的XML文件(随你觉得它是数据还是代码)
然后对于每个XML元素运行专门的Java代码来解释它. 它比听起来要简单得多. 一个简单的XML指令如下所示, 
它能让一个同名的Java类被加载并执行代码.

@pyblock{
<copy todir="../new/dir">
    <fileset dir="src_dir"/>
</copy>
}

这个片段将文件从一个目录复制到另一个. Ant定位"@code{copy}"任务(实际上是一个Java类), 
用相应的方法设置适当的参数(@code{todir}和@code{fileset})然后执行任务. Ant有一系列核心任务. 
任何人都可以用遵循一定协议的Java类轻松地扩展自己的任务, 并使其在解释器遇到名字对应的XML元素时被执行. 
非常简单. Ant高效地完成了我们在前文所提到的: 它看起来就是一个以XML为语法, 
将XML元素翻译到相应Java指令的解释器. 我们可以写一个"@code{add}"任务, 让Ant在见到前文展示的加法XML片段时执行它! 
考虑到Ant是一个流行的项目, 前面的主意看起来合理了一些, 毕竟它每天被数以千计的公司使用!

现在我还没说Ant为什么非要使用XML. 也别试着在它的网站上找答案. 你什么有用的都找不到. 
不管怎样, 它们与这里的讨论无关. 让我们再进一步. 是时候找到原因了.

@section{为什么是XML?}

有时明智的抉择不经过下意识的思考而作出. 我不确定James十分知道他为什么选择了XML - 这很可能是无意识的决定. 
至少, 我在Ant的网站上看到的其实都是错误的原因. 它说主要的原因在于可移植性和可扩展性. 
我看不出来XML是如何实现Ant的这些目标的. 和简单地执行Java代码相比, 解释XML有什么好处? 
为什么不用一些常用API(复制目录, 编译等)构造一组类, 然后直接以Java源码使用它们呢? 
这可以在任何Java支持的平台(Ant也如此要求)上运行, 还是可以无限扩展的, 并且它有更熟悉的语法. 
所以为什么是XML? 我们能找到一个合理的原因吗?

我们可以(尽管我说过我不确定James是否考虑过). XML在引入语义构造时拥有远超Java的灵活性. 
别担心我在用空话描述费解的概念. 这其实应该是相当简单的想法, 尽管要花一些功夫解释. 
扣好你的安全带. 我们在涅槃之路上要迈出一大步了.

如何在Java中表示'@code{copy}'呢? 下面是一种方法:

@pyblock{
CopyTask copy = new CopyTask();
Fileset fileset = new Fileset();

fileset.setDir("src_dir");
copy.setToDir("../new/dir");
copy.setFileset(fileset);

copy.execute();
}

这段代码几乎是一样的, 尽管比原来的XML长了一点. 所以区别在哪? 答案是XML片段为复制引入了一种新的语义构造. 
如果在Java中它会长得像这样:

@pyblock{		
copy("../new/dir")
{
    fileset("src_dir");
}
}

你能看出区别吗? 上面的代码(如果在Java中可能的话)是一个复制文件的特殊操作符 - 与for循环或Java 5中新出现的foreach循环相似. 
如果我们有一个XML到Java的自动转换器它会产出上面那坨狗屁不通的东西. 
原因在于Java的语法树是在语言规范中被写死的 - 我们不能修改它. 我们可以添加包, 
类, 方法, 但我们不能扩展Java以实现新的操作符. 然而我们可以在XML中尽情做到这一点 - 它的语法树不被解释器以外的任何东西束缚! 
如果道理还不够明白, 考虑下面的特殊Java操作符'@code{unless}':

@pyblock{				
unless(someObject.canFly())
{
    someObject.transportByGround();
}
}

在前面的两个例子中我们扩展了Java以引入新的文件复制操作符并加入了条件操作符@code{unless}. 
我们通过修改Java编译器的抽象语法树以实现它们. 我们自然不能用标准Java功能做到这一点, 
但我们可以在XML里轻易做到. 因为XML解释器解析产生的语法树, 我们可以使用任意的操作符.

对于复杂的操作这一能力提供了巨大的好处. 你能想象编写特殊操作符以签出源码, 编译文件, 
运行单元测试, 还有发送邮件吗? 尝试想一些. 如果你要处理特定的问题(此处是构建项目)
这些操作符可以奇迹般地减少键入的代码量并增加清晰度和可复用性. 解释XML使之非常易于实现, 
因为它是储存层级数据的简单文件. 我们在Java中实现不了这个操作符因为它的层级结构是固定的(你将会发现, 我们在Lisp中能实现). 
也许这是Ant成功的原因之一?

我建议你关注一下Java和C#(尤其是最新发布的C#3.0规范)最近的演进. 这些语言在迭代中抽象常见操作并将其以操作符的形式加入. 
新的C#内建查询操作符就是一个例子. 这是以一种传统方法实现的: 语言创造者修改语法树, 加入某些特性的实现. 
想象程序员能自己修改抽象语法树的可能性! 新的语言可以为特定领域(比如用于构建项目的语言, 就像Ant). 
你能想出更多例子吗? 考虑一下这些概念, 但不必太担心它们. 我们会在引入更多思想后回来. 到时候事情就会变得更清晰的.

@section{快到Lisp了}

让我们暂且忘了操作符的事, 试着把视野放到Ant的设计之外. 
我之前提到过Ant可以用遵守一定协议的Java类扩展. 
Ant解释器会试着用名字适当的Java类匹配XML元素. 如果匹配成功, 任务会被执行. 
一个有趣的问题出现了. 为什么不用Ant自身来扩展Ant呢? 毕竟, 
核心任务包含了许多程序语言应有的构造(@code{if}就是一个例子). 
如果Ant提供了在Ant自己中开发任务的构造, 我们就会得到更高的可移植性. 
我们会仅依赖一组核心任务(可以看作标准库), 不关心有没有Java运行时: 
核心任务可以用任何东西实现. 剩下的任务可以基于Ant-XML自身实现. 
Ant回成为一个泛用, 可扩展, 基于XML的编程语言. 考虑下面的可能性:

@pyblock{
<task name="Test">
    <echo message="Hello World!"/>
</task>
<Test />
}

如果Ant支持"@code{task}"构造, 上面的例子会打印"Hello World!". 
事实上, 我们可以在Java中写一个"@code{task}"任务, 
然后让Ant能用Ant-XML扩展自身! Ant会可以基于简单原语构建更复杂的, 
就像任何编程语言一样! 这是一个我们在教程开头讲到的基于"XML"的编程语言. 
不怎么实用(你知道为什么吗?), 但贼他妈酷.

顺便再看一眼我们的'@code{Test}'. 恭喜. 你在看Lisp代码. 这是在说什么? 
它看起来像Lisp吗? 别担心, 我们很快会弄明白. 感到奇怪? 很好. 让我们把它理干净!

@section{更好的XML}

我在前文提到自我扩展的Ant不怎么实用. 原因在于XML的繁冗. 
它作为数据文件还过得去, 但一旦你试图用它写复杂的程序, 
打字的负担会很快造成麻烦, 使之难以用于任何真的项目. 
你写过Ant构建脚本吗? 我写过, 只要它变得足够复杂, 
XML会显得很烦人. 想象一下为了闭标签把Java里的东西都打两遍.
这不会让你发疯吗?

解决方案是使用一种不那么冗杂的替代语法. 记住, 
XML只是一种展示层级结构的数据格式. 我们不必用XML的尖括号序列化树. 
我们可以找些其他格式. 一种格式(正好是Lisp使用的)叫S-表达式. 
S-表达式和XML实现一样的目标. 它不过更加简明, 适于代码键入. 
我会花一点时间解释S-表达式, 但在此之前我要澄清关于XML的一些事情. 
让我们考虑复制文件的XML示例.

@pyblock{
<copy todir="../new/dir">
    <fileset dir="src_dir">
</copy>
}

思考一下这份片段在内存中会表示为怎么样的树.
我们会得到一个包含fileset节点的'copy'节点. 但属性呢? 
它在这里作为什么? 如果你用XML描述过数据并疑惑该用元素还是数学, 
你不是一个人. 没有人能真的告明白. 处理塔的方法更像黑魔法而非科学. 
这是因为属性实际上是元素的子集. 属性能做的, 元素都能做. 
之所以XML引入了属性是为了降低它的繁冗. 看一下'copy'的另一个版本:

@pyblock{
<copy>
    <todir>../new/dir</todir>
    <fileset>
        <dir>src_dir</dir>
    </fileset>
</copy>
}

两段XML包含的信息是完全一样的. 然而, 我们用属性避免把同样的东西打两遍. 
想象一下属性不在XML的规范中. 任何XML都能把人逼疯!

知道属性的本质后, 让我们回到S-表达式. 
之所以要绕这个弯子是因为S-表达式没有属性. 因为它足够简洁, 
属性是不需要的. 这是我们在将XML转化为S-表达式时需要牢记的. 
看看这个例子. 我们可以把上面的XML翻译成这样的样子.

@codeblock{
(copy
    (todir "../new/dir")
    (fileset (dir "src_dir")))
}

好好端详这个表示法. 区别在哪? 尖括号被圆括号替代了. 
我们不用一对括号包围, 然后用类似于"(/element)"东西闭合. 
我们跳过"(element"的第二个括号. 元素接着被")"闭合. 
就是这样! 翻译既自然又简单. 它还更方便打字. 括号会弄瞎初学者吗? 
可能吧, 但既然明白了背后的道理它们也易于处理了. 
至少它比让人指节发炎的XML强. 适应了用S-表达式写程序后, 它不仅可行还很舒服. 
并且它提供了XML代码都所有好处(其中许多仍需我们探索). 
让我们看一下更像Lisp的'task'版本:

@codeblock{
(task (name "Test")
    (echo (message "Hello World!")))

(Test)
}

S-表达式在Lisp术语中叫列表. 考虑上面的'task'元素. 如果我们去掉换行, 
用逗号替代空格, 它看起来出奇地像一个包含元素和其他列表的列表
(我加入了格式化以方便看出嵌套列表):

@codeblock{
(task, (name, "test"), (echo, (message, "Hello World!")))
}

我们在XML中也能做到一样的事. 当然上面这一行不是真的列表. 它是树, 
就像XML中的一样. 不要被列表的名字误导了, 因为嵌套其他列表的列表就是树. 
Lisp代表列表处理(List Processing), 但它实际是树处理 - 和处理XML节点并无不同.

漫游良久后我们终于见到了看起来像(实际上也是)Lisp的东西. 
现在神秘的Lisp括号和Lisp教徒的宣言更容易理解了. 但我们还要提到许多东西. 
准备好了? 出发!