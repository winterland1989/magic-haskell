透镜组
======

透镜组这个概念源自之前建立在记录语法之上的数据操作函数，主要的目的是方便不可变数据的操作。使用`data`关键字定义数据类型的时候，一种定义方式是使用记录语法，给构造函数的每一个数据项添加标签，来方便对应数据结构中对应数据项的提取和更新操作。Haskell的记录语法提供的数据操作方案是根据标签生成对应的提取和更新函数，这个方案在遇到复杂数据结构的时候会遇到难以书写和优化的问题。本章介绍的透镜组使用函子抽象来构造数据操作函数，是目前Haskell中操作复杂数据结构的首选方案。

getter setter
-------------

getter和setter并不是在Haskell中出现的概念，而是在很多面向对象语言中，用来操作对象实例的一个语法，以JavaScript为例：

```js
position = {x: 1, y: 2}
position.x
// 1

position.y = 3
// position == {x: 1, y: 3}
```

简单的说，在大部分面向对象语言中，如果想取出一个对象x的某个属性p，只需要使用`x.p`即可，而如果希望更改某个对象的属性p，则直接对`x.p`赋值，而有时候你不希望直接暴露属性p，那么你可以给对象添加两个方法`x.getP`和`x.setP`，通过调用这两个方法，你可以完成读取和更改的操作，这两个函数很多时候被称为getter和setter。

这条不成文的规定到了Haskell这里成了另外一个故事，我们在定义数据类型的时候，有下面这样的记录语法：

```
data Position = Position { positionX :: Double, positionY :: Double }

p = Position 1 2

positionX p
-- 1

p2 = p { positionY = 3 }
-- Position 1 3
```

看起来还不错嘛，只是因为在Haskell中绑定是不会改变的，所以`p2`创建了一个新的`Position`记录，这个记录的值是我们希望更新后的值，而在内存中，情况大约如下：


```ascii

        +---------------+
    p-->| :: Position   |
        +---------------+
        |   *   |   *   +-+
        +---+---+-------+ |
            |             |
            V             V
        +-----------+   +-----------+  +-----------+ 
        | ::Double  |   | ::Double  |  | ::Double  |
        +-----------+   +-----------+  +-----------+
        |     1     |   |     2     |  |     3     |
        +-----------+   +-----------+  +-----------+
            ^                                ^
            |                                |
        +---+---+-------+                    |
        |   *   |   *   +--------------------+
        +---------------+
    p2->| :: Position   |
        +---------------+

```

也就是说，因为在Haskell中所有绑定都不会改变，所以我们并不会浪费内存去复制`p1`和`p2`中相同的元素，`p1`和`p2`共享了相同的数据`1 :: Double`，实际上刚刚的记录的更新表达式是下面表达式的语法糖：

```
p2 = Position {
        positionX = positionX p
    ,   positionY = 3
    }
```

在这里我们新建了一个`Position`盒子，把盒子的内容指向对应的位置，这些都是模式匹配和构造函数的语义。我们实际上可以把下面两个函数当作是Haskell世界里的getter和setter：

```
positionX :: Position -> Double
positionX (Position x _) = x

setPositionX :: Double -> Position -> Position
setPositionX x' p = p { positionX = x' }
```

其中`positionX`是记录语法提供的提取数据的方法，而`setPositionX`简单地把记录语法中更新数据地部分绑定到了函数上，这个函数接收一个`Double`类型地值`x'`作为新的横坐标，一个`Position`类型地值`p`作为原先地点，然后返回一个新的`Position`类型的值，这是一个横坐标更新之后的坐标值，这两个函数提供了操作坐标中横坐标的能力。

记录语法提供的getter和setter的解决方案在大部分情况下都可以胜任要求，但是当数据结构变的愈加复杂的时候，问题出现了，假定现在我们定义了一个新的数据类型`Line`用来表示直角坐标系上的一条线段，这条线段由起点`lineStart`和终点`lineEnd`共同确定：

```
data Line = Line { lineStart :: Position, lineEnd :: Position }

line1 = Line (Position 0 0) (Position 3 4)
```

现在我希望能够调整`line1`的终点纵坐标从4变到到5，应该如何做呢？我们有几种选择：

```
-- 模式匹配
line2 = case line1 of Line p1 (Position x _) -> Line p1 (Position x 5)

-- 记录语法
line2 = line1 { lineEnd = (lineEnd line1) { positionY = 5 } }

-- getter setter
line2 = setLineEnd (setPositionY 5 (lineEnd line1)) line1
```

在模式匹配中，我们需要手动匹配线段的起点`p1`并在重建线段的时候使用它，而在记录语法里，我们需要手动提取`line1`的终点`lineEnd line1`，才能让接下来的更新操作得以继续。显然不管是模式匹配还是记录语法，我们都需要写出一堆辅助的表达式，而实际上这些额外的绑定都是不需要，这样添加绑定然后扔掉的行为既浪费运行时，写起来又很麻烦。假如数据的层级变得更深的时候，可以想象我们每次操作一个深层次数据时需要添加多少层级的辅助操作，我们必须有一个高效、优雅的解决方案来解决函数式编程中嵌套数据结构操作的问题。

Lens
-----

现在请做好准备，让我们来介绍Haskell中的一个想象力非凡的概念：透镜组`Lens`。顾名思义，透镜组的作用是可以让你更清楚地观察，而在光学上透镜组可以相互组合，构成新的透镜组，从而获得望远镜、显微镜等等满足各种观察要求的透镜组，对于Haskell来说，透镜组的作用，就是让你透过它，操作复杂的数据结构中的一小部分，而在Haskell中，透镜组最重要的特点，就是可以组合！

让我们先来看看它是如何被定义的：

```
type Lens b a = Functor f => (a -> f a) -> b -> f b
```

`Lens b a`是一个类型别名，它指的是类型是`(a -> f a) -> b -> f b`的函数，这个函数里面的`a`类型是数据中你需要操作的那部分的类型，而`b`类型则是数据本身的类型，所以对于`Position`类型来说，我们可以定义出两个透镜组：

```
xLens :: Functor f => (Double -> f Double) -> Position -> f Position
yLens :: Functor f => (Double -> f Double) -> Position -> f Position
```

由于类型别名中并没有包含函子的类型变量`f`，所以想要编译器接受这个类型，需要打开高阶类型的扩展，我们暂时先不用理会这个限制，只需关心怎么写出具体的透镜组。我们以`xLens`为例，`xLens`会接收到了一个`Double -> f Double`类型的函数，然后把这个函数变成一个`Position -> f Position`类型的函数，在没有其他任何信息的情况下，唯一的线索是`Lens`类型别名中的一个约束`Functor f =>`，这要求`f`一定要是一个函子类型，而我们手上现在掌握的函数中，下面两个能够反映`Double`类型的横坐标和`Position`类型的值的关系：

```
positionX :: Position -> Double
setPositionX :: Double -> Position -> Position
```

我们还有一个可以把`Double -> Position`类型升格成`f Double -> f Position`类型的函数`fmap`，这个`fmap`的具体定义我们并不关心，这和`f`的具体类型相关，但是我们却可以利用这个函数定义出我们需要的`Functor f => (Double -> f Double) -> Position -> f Position`类型的函数：


```
xLens :: Functor f => (Double -> f Double) -> Position -> f Position
-- 也可以使用类型别名，注意类型别名中，大类型在前，小类型在后
-- xLens :: Lens Position Double

xLens f p = fmap (\x' -> setPositionX x' p) $ f (positionX p)
  where
    setPositionX :: Double -> Position -> Position
    setPositionX x' p = p { positionX = x' }

-- inline写法
xLens f p = fmap (\x' -> p { positionX = x' }) $ f (positionX p)

```

注意`xLens`做的事情：

+ 接收的参数中，`f`是一个把`Double -> f Double`类型的函数，`p`是`Position`类型的数据。

+ 通过`positionX p`把`p`中的横坐标提取出来，并交给`f`得到一个包裹在函子中的值，`f (positionX p) :: f Double`。

+ 通过构造匿名函数`\x' -> setPositionX x' p`得到一个`Double -> Position`类型的函数，这个函数接收一个`x'`并把它当成新的横坐标设置给`p`。

+ 最后我们通过`fmap`把刚刚构造出来的`Double -> Position`类型的函数升格为`f Double -> f Position`类型的函数，并把第二步得到的包裹在函子中的值`f (positionX p)`交给这个函数，注意`$`的使用。

+ 我们成功地得到了函数类型说明中要求得到的`f Position`类型的值。

你可能在想这个函子类型`f`有什么作用？让我们来看看：

```bash
Prelude> data Position = Position { positionX :: Double, positionY :: Double } deriving Show
Prelude> data Line = Line { lineStart :: Position, lineEnd :: Position } deriving Show
Prelude> let setPositionX x' p = p { positionX = x' }
Prelude> let let xLens f p = fmap (\x' -> setPositionX x' p) $ f (positionX p)
Prelude> xLens (\x -> Just (x+1)) (Position 3 4)
Just (Position {positionX = 4.0, positionY = 4.0})
Prelude> xLens (\x -> Nothing) (Position 3 4)
Nothing
Prelude> xLens (\x -> [x+1, x+2, x+3]) (Position 3 4)
[ Position {positionX = 4.0, positionY = 4.0}
, Position {positionX = 5.0, positionY = 4.0}
, Position {positionX = 6.0, positionY = 4.0} ]
```

记得使用`deriving Show`可以自动生成数据类型的`Show`实例，从而在GHCi中方便地显示出来，我们给`xLens`传递`Double -> Maybe Double`类型的函数，代表我们在更新坐标地时候计算可能会失败，结果我们得到了`Maybe Position`类型的值，而不是包含`Maybe Double`的`Position`，当我们丢给`xLens`的函数是`Double -> [Double]`类型的时候，代表我们可能在一次操作中产生若干个新的横坐标，我们得到了每一个新的横坐标对应的新坐标！

通过这个例子我们看到了透镜组`xLens`的威力，它允许你使用函子来包裹你的数据操作，从而获得各种各样的计算语义，这其中的核心，是不同的函子类型实例声明中不同的`fmap`实现，下面问题来了，我们能否通过透镜组获得getter和setter？


view set over
-------------

上面说到的`Lens`类型，最早出现在2009年Twan van Laarhoven的一篇文章[^1]中，当时他并没有把这个类型的函数叫做`Lens`，而他当时关注的重点，是如何运用合适的函子类型`f`，获得getter和setter，在上一章我们介绍的两个函子`Identity`和`Const a`这个时候扮演了至关重要的角色，首先我们定义我们希望得到的getter和setter：

```
view :: Lens Position Double -> Position -> Double
set :: Lens Position Double -> Double -> Position -> Position
over :: Lens Position Double -> (Double -> Double) -> Position -> Position
```

我们希望能够从透镜组中得到的一组函数分别是：

+ `view xLens`应该得到`Position -> Double`类型的函数，也就是从坐标中提取出横坐标的函数。

+ `set xLens`应该得到`Double -> Position -> Position`类型的函数，也就是设置坐标中的横坐标，并返回新坐标的函数。

+ `over xLens`应该得到`(Double -> Double) -> Position -> Position`类型的函数，这个函数接收一个针对横坐标的变换函数，并把变换作用在坐标中的横坐标上，返回新的坐标。

我们为什么如此确定透镜组`xLens`包含所有我们需要的信息呢？让我们把刚刚的`xLens`改写一下：

```
xLens f p = fmap setter $ f $ getter p
  where
    setter :: Double -> Position
    setter x' = p { positionX = x' }

    getter :: Position -> Double
    getter = positionX 
```

实际上为了能够把函子类型从小类型外层移到大类型的外层，我们即使用到了getter，又使用到了setter，而接下来我们将要向你展示，`view`、`set`、`over`这些函数，全部都是`Lens`类型函数的一个特例，我们需要做的，只是选择合适的函子类型来从`Lens`中提取我们想要的函数即可。有趣的是，上面三个函数中最容易实现的，是看上去最复杂的`over`函数：

###over###

```
over :: Lens b a -> (a -> a) -> b -> b
```

这里我们没有使用任何具体类型的透镜组，因为我们需要`over`能够工作在任意类型的透镜组上，所以也不能在这个函数中使用到任何涉及到具体数据类型的函数，因为我们正是要用透镜组来定义getter和setter！注意到上面的类型展开后是：

```
over :: Functor f => ((a -> f a) -> b -> f b) -> (a -> a) -> b -> b
over lens f x = ???
```

我们掌握的信息并不多，根据Twan的关键的观察，我们通过选择合适的函子类型来获得额外的信息，这里我们选择的函子是`Identity`，对于这个函子我们有下面两个函数：

```
newtype Identity a = Identity { runIdentity :: a}

Identity :: a -> Identity a
runIdentity :: Identity a -> a
```

我们的透镜组需要一个`a -> f a`类型的函数，而`over`接收到的`f`却是`a -> a`类型的，我们通过函数组合`(.)`把`f`和构造函数`Identity`组合起来：

```
over :: ((a -> Identityf a) -> b -> Identity b) -> (a -> a) -> b -> b
over lens f x = ???
  where
    lifted = lens (Identity . f) 
```

这个时候我们把组合出来的`a -> Identity a`的函数交给透镜组，实际上已经得到了`b -> Identity b`类型的函数了，而恰好我们手上有一个`b`类型的参数`x`，我们把`x`交给上面的`lifted`，即可得到一个`Identity b`类型的值，而这个值可以方便的通过`runIdentity`把包裹在`Identity`中的值提取出来，于是我们得到了最终的`over`：

```
over :: ((a -> Identityf a) -> b -> Identity b) -> (a -> a) -> b -> b
over lens f x = runIdentity $ lifted x
  where
    lifted = lens (Identity . f) 

-- 代入消除lifted
over lens f x = runIdentity $ lens (Identity . f) x

-- point-free写法
over lens f = runIdentity . lens (Identity . f)
```

我们去GHCi上面试试：

```bash
Prelude> import Data.Functor.Identity 
Prelude Data.Functor.Identity> let over lens f = runIdentity . lens (Identity . f)
Prelude Data.Functor.Identity> :t over
over
  :: ((a1 -> Identity a2) -> a -> Identity c) -> (a1 -> a2) -> a -> c
Prelude Data.Functor.Identity> over xLens (+1) (Position 3 4)
Position {positionX = 4.0, positionY = 4.0}
Prelude Data.Functor.Identity> over xLens (+1) (Position 4 4)
Position {positionX = 5.0, positionY = 4.0}
```

Nice!我们的`over`在不知道构成棱镜组的具体类型的情况下，仅仅凭借函子`Identity`的两个函数，就完成从棱镜组中提取`over`函数的任务！

###set###

下面我们来看和`over`相关的`set`的实现，这个实现其实相当简单，考虑到下面两个式子应该是等价的：

```
set xLens 3 p
over xLens (\_ -> 3) p
```

也就是说，`set`需要做的，就是不管之前坐标的横坐标是什么值，我们都把它设置为接收到的新值，于是我们很容易得出下面的解法：

```
set :: ((a -> Identity a) -> b -> Identity b) -> a -> b -> b
set lens a' x = over lens (\_ -> a') x

-- point-free写法
set lens a' = over lens (const a)

-- 回顾const的定义
const :: a -> b -> a
const x _ = x
```

我们在继续推导`view`的解法之前，先来回顾下`set`是如何工作的：

```
set xLens 3 (Position 1 2)

-- 展开set定义
over xLens (const 3) (Position 1 2)

-- 展开over的定义
runIdentity $ xLens (Identity . (const 3)) (Position 1 2)

-- 展开xLens定义
runIdentity (
    fmap 
        (\x' -> (Position 1 2) { positionX = x' })
        ((Identity . (const 3)) (positionX (Position 1 2)))
    )

-- 计算positionX (Position 1 2)
runIdentity (
    fmap 
        (\x' -> (Position 1 2) { positionX = x' })
        ((Identity . (const 3)) 1)
    )

-- 计算(Identity . (const 3)) 1
runIdentity (
    fmap 
       (\x' -> (Position 1 2) { positionX = x' })
        (Identity 3)
    )

-- 展开Identity的fmap定义
runIdentity (
    Identity ( (Position 1 2) { positionX = 3 } )
    )

算更新后的坐标
runIdentity (Identity (Position 3 2))
-- Position 3 2
```

看上去透过透镜组操作数据好像进行了很多的计算，但是实际上`Identity`函子是通过`newtype`定义出来的，所以表面的打包解包`Identity/runIdentity`在运行的时候根本没有发生，在底层`Identity Position`和`Position`类型的表示是完全一致的，而`xLens`的展开过程在编译的过程中就已经被优化掉了。

###view###

下面让我们来看看如何从一个透镜组中提取出getter，`view`函数做的就是这个事情：

```
view :: Lens b a -> b -> a
view lens x = ???
```

我们同样使用类型变量`b`和`a`分别代表大类型和小类型，这样和`over`一样，`view`函数也可以工作在任意类型组成的棱镜组之上，我们现在需要思考清楚的是，最后要得到的函数是`b -> a`类型的，也就是从大类型（例如Position）到小类型（例如Double），而对于棱镜组的类型`(a -> f a) -> b -> f b`来说，我们最后总会得到一个包裹在函子中的`f b`类型的值，我们如何从`f b`中得到`a`类型的值呢？

答案就隐藏在上一章提到的`Const a`类型的函子，对于这个类型的函子来说，`Const a b`类型的值其实根本就不包含`b`类型的值，而我们根据`Const a b`的定义，我们有下面的函数来提取盒子上面的`a`类型的值，而不是盒子里面不存在的`b`：

```
newtype Const a b = Const { getConst :: a }

Const :: a -> Const a b
getConst :: Const a b -> a

getConst (Const 3)
-- 3
```

我们现在选择`Const a`来作为`view`类型中`Lens b a`展开时的`f`函子，为的是和最后需要提取的`a`类型的值类型匹配：

```
view :: ((a -> Const a a) -> b -> Const a b) -> b -> a
view lens x = getConst ((lens Const) x)

-- point-free
view lens = getConst . (lens Const)
```

注意到构造函数`Const`的类型是`Const :: a -> Const a b`，而`b`是和`a`不相关的类型变量，这里我们就选择`b`等于`a`，`lens`接收到`a -> Const a a`类型的构造函数`Const`之后，得到了`b -> Const a b`类型的函数，我们把`b`类型的值`x`传给这个函数得到包裹在`Const a`中的`Const a b`类型的值，这个时候我们得到了需要的盒子`Const
a`，于是使用`getConst`提取盒子上面的`a`类型的值即可。我们用`xLens`为例，试着推导一下：

```
view xLens (Position 1 2)

-- 展开view定义
getConst ((xLens Const) (Position 1 2))

-- 展开xLens定义
getConst (
    fmap (\x' -> (Position 1 2) { positionX = x' })
        (Const (positionX (Position 1 2)))
    )

-- 计算Const
getConst (
    fmap (\x' -> (Position 1 2) { positionX = x' })
        (Const 1)
    )

-- 根据Const的fmap定义，直接忽略fmap后的函数
getConst (Const 1)
-- 1
```

我们成功地使用`view xLens`获得了`Position -> Double`类型的getter函数，用来提取坐标中的横坐标。和`set`、`over`一样，`view`函数推导过程中打包解包`Const`的过程实际上在运行的时候也并不并会发生。

最后，让我们回到最初的问题上，来看看透镜组是如何解决深层次数据操作问题的，我们在声明数据类型的时候，不再声明`getXXX`和`setXXX`的函数，而是声明`xxxLens`这样的透镜组：

```
data Position = Position { positionX :: Double, positionY :: Double }
data Line = Line { lineStart :: Position, lineEnd :: Position }

xLens :: Lens Position Double
xLens f p = fmap (\x' -> p { positionX = x' }) $ f (positionX p)
yLens :: Lens Position Double
yLens f p = fmap (\y' -> p { positionY = y' }) $ f (positionY p)

startLens :: Lens Line Position
startLens f l = fmap (\s' -> l { lineStart = s' }) $ f (lineStart l)
endLens :: Lens Line Position
endLens f l = fmap (\s' -> l { lineEnd = s' }) $ f (lineEnd l)
```

注意下面两个类型的特点：

```
yLens :: Lens Position Double
yLens :: Functor f => (Double -> f Double) -> Position -> f Position

endLens :: Lens Line Position
endLens :: Functor f => (Position -> f Position) -> Line -> f Line
```

`yLens`和`endLens`不过就是高阶函数，而`yLens`的返回值类型和`endLens`需要的参数类型正好相同，所以我们可以使用组合函数`(.)`连接它们：

```
endLens . yLens :: Functor f => (Double -> f Double) -> Line -> f Line
endLens . yLens :: Lens Line Double
```

我们得到了从线段终点纵坐标到线段本身的透镜组！所以我们可以像使用其他透镜组一样通过`view/set/over`来使用它：

```
line1 :: Line
line1 = Line (Position 0 0) (Position 3 4)

set (endLens . yLens) 5 line1
-- Line (Position 0 0) (Position 3 5)

view (endLens . yLens) line1
-- 4
```

这也是透镜组这个名字有趣的地方，我们把透镜组中小类型称作是对应大类型的焦点（focus），通过一系列焦点和透镜组的组合，我们便可以得到任意深层次的透镜组，为了进一步方便我们书写，我们可以定义下面的中缀函数：

```
-- 中缀版本view
(^.) :: b -> Lens b a -> a
x ^. lens = view lens x
-- point-free
(^.) = flip view

infixl 8 ^.
-- 优化级比合成函数(.)低：infixr 9 .

line1 ^. endLens . yLens
-- 4

-- 中缀版本over
(%~) :: Lens b a -> (a -> a) -> b -> b
lens %~ f x = over lens f x
-- point-free
(%~) = over

infixr 4 %~

line1 & endLens . yLens %~ (^2)
-- Line (Position 0 0) (Position 3 16)

-- 中缀版本set
(.~) :: Lens b a -> a -> b -> b
lens %~ a' x = set lens a' x
-- point-free
(.~) = set

infixr 4 .~

line1 & endLens . yLens .~ 10
-- Line (Position 0 0) (Position 3 10)
```

除了`^.`之外，这些中缀函数大多需要两个以上的参数，所以我们使用`&`管道函数把参数从左往右送过去，`&`的优先级定义是`infixl 1`，所以以上面`.~`的式子为例，我们这么来理解：

```
-- 回顾(&)的定义
(&) :: a -> (a -> b) -> b
x & f = f x

line1 & endLens . yLens .~ 10
-- 根据优先级
-- ((endLens . yLens) .~ 10) line1
-- 根据.~定义
-- set (endLens . yLens) 10 line1
```

而对于`^.`的point-free的写法中用到的`flip`，对比`^.`和`view`的类型，读者可以猜出应该是这样一个函数了：

```
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
```

函数库
------

上面提到的中缀函数，以及各种常见的数据类型的透镜组，在Hackage上面有几个著名的函数库已经都帮助你定义好了，而且生成自定义数据类型的透镜组的工作也不需要你手动完成，因为Haskell中有模版（template）编程的功能，只要你提供了使用记录语法定义好的数据类型，一句话就可以让编译器自动帮你生成记录中每一项的透镜组。这些我们在后面使用到的时候再讲。

而实际上的透镜组的类型往往要比本文中提供的要复杂些，例如在lens函数库里，透镜组是这样定义的：

```
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
```

这里我们把数据操作过程的类型从`a -> f a`扩展到了`a -> f b`，表示在操作子数据的时候，类型有可能已经发生了改变，于是大类型`s`也可能发生相应的改变，变成类型`t`，而对于本文中出现的简单的数据操作的情况，我们用`Lens' s a`表示上述操作的一个特例，关于`Lens`其实可以说的还有很多，这里只是提供了一个基础的视角，我们在后面会再次回到这个类型，讲述它的其他使用方法。

文章中还展示了Haskell中一个实现函数常用的思路：先写出你要的函数类型，然后根据类型去推导，利用已知的函数，去一点点拼凑出想要的函数，而这个函数最终的底层实现过程，在函数被实现之后再慢慢分析即可。很多时候，当类型确定的时候，函数的实现就已经确定了。所以在Haskell中，设计类型是编程过程中非常重要的一步，这是其他编程语言很难带来的体验，而编译器强大的类型推断和检查，能够保证你的思路不会出错。当然，想要写出`(a -> f a) -> b -> f
b`这样的类型，需要敏锐的洞察力和灵感，这正是函数式编程的美妙之处。

[^1]: CPS based functional references, http://www.twanvl.nl/blog/Haskell/cps-functional-references
