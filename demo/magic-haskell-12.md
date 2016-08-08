应用函子
========

应用函子是出现较晚的抽象类型类，在GHC7.10中正式成为“函子 -> 应用函子 -> 单子”类型类层级的一部分，作为函子的子类型类，它概括了某些函子的额外特性：把函子里的函数和到函子里的值可以得到新的函子。并不是每个函子都可以提供这样的函数，那么这样的函数有什么用处，在应用函子里又是怎么表示的呢？本章的内容将会回答这两个问题，

函子的局限
---------

在前面讲述函子的章节，我们说函子抽象提供了`fmap`函数，用来把普通函数升格成可以操作函子容器的函数，其中举的一个例子是如果有一个不确定是否有值的`Maybe a`类型的值，我们能够把所有能够处理`a`类型的函数升格成处理`Maybe a`类型的函数：

```
maybeThree = Just 3
maybeFour = Nothing

fmap (+1) maybeThree
-- Just 4

fmap (+1) maybeFour
-- Nothing
```

假设现在有一个`a -> b -> c`类型的函数，而参数中有一个`Maybe a`类型的值，和一个`b`类型的值，我们可以继续使用`fmap`：

```
a :: Maybe Int
a = Just 3

b :: Char
b = 'x'

replicate :: Int -> b -> [b]

replicateB :: Int -> String
replicateB = \x -> replicate x b

fmap replicateB a
-- Just "xxx"
```

这里我们通过构造了`replicateB`这个函数，使得`fmap replicateB`的类型变成了`Maybe Int -> Maybe String`，从而得到了复制之后的字符串。

但是实际上遇到的问题，往往是参与计算的参数里不只有一个是包裹在函子类型里面的，假如上面的例子中`Char`类型的值也被包裹在了`Maybe`里，我们如何继续把`replicate`作用在`Maybe Int`和`Maybe Char`上得到`Maybe String`呢？


```
a :: Maybe Int
a = Just 3

b :: Maybe Char
b = Just 'x'

fmap replicate ???
```

第一个想法是回到最原始的模式匹配，我们总可以把`Maybe`分成两种情况考虑：

```
replicateMaybe :: Maybe Int -> Maybe a -> Maybe [a]
replicateMaybe (Just n) (Just a) = Just $ replicate n a
replicateMaybe Nothing _ = Nothing
replicateMaybe _ Nothing = Nothing

replicateMaybe a b
-- Just "xxx"

replicateMaybe a Nothing
-- Nothing
```

不管两个参数中哪一个是`Nothing`，我们都无法完成计算，所以直接返回`Nothing`，而如果两个参数都是通过`Just`构造的，那么我们提取其中的值交给`replicate`，最后把结果包裹到`Just`里。

我们把上面这个过程抽象出来，可以得到一个用来升格双参数函数的高阶函数`liftMaybe2`：

```
liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe2 f (Just x) (Just y) = Just $ f x y
liftMaybe2 _ _ _               = Nothing

liftMaybe2 replicate a b
-- Just "xxx"
```

类似我们可以得到`liftMaybe3`、`liftMaybe4`...这样可以处理任意个参数是`Maybe`的情况，但是这么做有两个问题：

+ 我们需要为每一种参数数量的情况书写一个特定的函数，这个过程机械而无趣。

+ 我们无法使用别的类型的函子，例如假设参数都是装在列表中的话，我们还需要书写`liftList2`、`liftList3`...。

这迫使我们思考一个更加通用的解决方法，可以方便的把函数升格至可以适应任意数量的、装在函子中的参数情况。

这时我们需要一个关键的观察：如果多参数函数只经过`fmap`的升格，并部分应用一个包裹在函子中的值，我们将会得到一个包裹在函子中的函数：

```
replicate :: Int -> a -> [a]
replicate :: Int -> (a -> [a])

fmap replicate :: f Int -> f (a -> [a])
fmap replicate (Just 3) :: Maybe (a -> [a])
```

这里我们把`replicate`看作是一个接收`Int`类型参数，并返回`a -> [a]`类型函数的一个高阶函数，经过`fmap`升格之后，函数的类型已经变成了`f Int -> f (a -> [a])`，如果我们这时给它传递一个`Maybe a`类型的值，我们就会得到了一个包裹在函子中的函数`Maybe (a -> [a])`，可是我们如何继续使用这个函数呢？实际上，一旦我们解决了如何让`f (a -> [a])`类型的函数和`f a`类型的值进行应用的问题，我们之前的问题就引刃而解了：

```
replicateThreeF :: Maybe (a -> [a])
replicateThreeF = fmap replicate (Just 3)

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe (Just f) (Just x) = Just $ f x
applyMaybe _ _               = Nothing


applyMaybe replicateThreeF (Just 'x')
-- Just "xxx"

applyMaybe replicateThreeF Nothing
-- Nothing
```

`applyMaybe`做的事情很简单，先判断两侧的函子中是否都有想要的函数`f`和参数`x`，有的话就直接用`f`处理`x`并包回到`Just`中，否则任意一个条件不足都会返回`Nothing`。这里需要理解的是`Maybe (a -> b)`这个函子里包含的函数有可能不存在，对比下面两个包裹在`Maybe`中的部分函数：

```
replicateThreeF :: Maybe (a -> [a])
replicateThreeF = fmap replicate (Just 3)

replicateNothing :: Maybe (a -> [a])
replicateNothing = fmap replicate Nothing

applyMaybe replicateNothing (Just 'x')
-- Nothing
```

如果我们交给`fmap replicate`的参数是`Nothing`的话，我们得到的就是一个`Nothing`，按照类型来说，我们得到的是一个包裹在函子中的`Maybe (a -> [a])`，而实际上我们并没有。

我们有了`applyMaybe`这个函数之后，就可以实现升格任意参数数量的函数到`Maybe`参数类型上了：

```
addAll :: Int -> Int -> Int -> Int
addAll x y z = x + y + z

(fmap addAll $ Just 1) `applyMaybe` Just 2 `applyMaybe` Just 3
-- 6
```

注意在第一次使用`fmap`升格之后，`fmap addAll $ Just 1`的类型是`Maybe (Int -> Int -> Int)`，而这时我们把`Int -> Int -> Int`再次当成接收`Int`并返回`Int -> Int`的函数，于是``` `applyMaybe` Just 2```的结果是一个`Maybe (Int -> Int)`类型的函数，最终我们通过``` `applyMaybe` Just 3```把最后一个包裹在函子中的参数交给了之前部分应用得到的函数，从而得出答案，在这个过程中，任意个地方出现`Nothing`都会导致最终的结果失败：

```
(fmap addAll $ Nothing) `applyMaybe` Just 2 `applyMaybe` Just 3
-- Nothing

(fmap addAll $ Just 1) `applyMaybe` Just 2 `applyMaybe` Just 3
-- Nothing

(fmap addAll $ Just 1) `applyMaybe` Just 2 `applyMaybe` Nothing
-- Nothing
```

假如我们在计算过程中，有一个参数没有包裹在函子中，对于`Maybe a`类型来说，我们只需要把它用`Just`包裹起来即可，因为一个确定的值是不会失败的。所以使用`applyMaybe`可以随意地把一个函数作用在了包裹在`Maybe`中，或者没有任何包裹的参数上。问题是我们可否抽象出一个适用于任意类型函子`f`上的函数呢：

```
(<*>) :: Functor f => f (a -> b) -> f a -> f b
(<*>) = ???
```

为了书写方便，我们把应用的过程写成中缀函数`<*>`，这个函数不仅仅可以接收`Maybe (a -> [a])`，还可以接收任意包裹在函子中的函数`f (a -> b)`，所以它又被称为函子应用运算符，现在让我们试试列表，这是另一个很好理解，而且有用的应用函子：

```
(<*>) :: [(a -> b)] -> [a] -> [b]
fs <*> xs = concat $ map (\f -> map f xs) fs
```

其中concat是连接列表的列表的函数：

```
concat :: [[a]] -> [a]
concat xss = foldl (++) [] xss
```

我们用`foldl (++) []`，把一个列表的列表中所有的子列表从左向右连接了起来，当然我们有更加高效的方法达成同样的效果，这里只是示范`<*>`在列表函子的情况下会表现出什么行为：

+ 首先`map (\f -> map f xs) fs`把`fs`中每一个函数`f`交给了匿名函数`\f -> map f xs`。

+ 匿名函数`\f -> map f xs`把函数`f`作用在了`xs`中的每一个`x`上，得到处理后的子列表。

+ 现在处理后的子列表被映射回列表的列表，类型是`[[b]]`，我们使用`concat`把所有自列表连接起来，得到最终的结果。

这个`<*>`在列表上的表现和`fmap`很像，就是把左侧列表中的函数和右侧列表中的参数都拿出来，分别相互作用，并把作用的结果全部返回成一个新的列表。

```
[(*1), (*2)] <*> [1,2,3]
-- [1,2,3,2,4,6]

fmap replicate [1,2,3] <*> ['x', 'y', 'z']
-- ["x","y","z","xx","yy","zz","xxx","yyy","zzz"]
```

这在很多动态规划的问题上看来很有用处，因为我们现在可以方便的计算出在全部输入条件和全部计算情况下的所有可能结果，如果计算中任意一步返回`[]`，整个结算也将返回`[]`。这里值得注意的是，我们仍然遇到了和`Maybe`同样的问题，就是计算中途遇到了一个没有包裹在列表中的参数怎么办？从计算的结果上分析，我们只需要把这个参数包裹到一个列表中，构成一个长度为1的单元素列表，计算就可以继续下去了，同时不会影响计算结果。这和`Maybe`中把确定的参数用`Just`来包裹道理是一样的，我们需要一个最简单的操作让一个值升格成为包裹在函子中的值，使得`<*>`可以连接它们，但同时又不能影响到`<*>`确定下来的计算的语义。这个升格参数的操作我们在下面会提到。

回到上面的问题，我们可否抽象出一个适用于任意类型函子`f`上的函数`<*>`呢？答案是不可以，证明这类问题最简单的办法是举出一个反例，这里以之前的`Const a`函子为例：

```
(<*>) :: Const a (b -> c) -> Const a b -> Const a c
cf <*> cx = ??? 
```

这个奇怪的容器里面其实什么都没有，所以我们既没有函数，也没有参数。但是问题并不只是出现在这里，我们最后返回的盒子也并不需要装进一个`c`类型的值，所以我们不用计算出值。问题在于，我们手上接收到了两个盒子，每个盒子外面各携带了一个`a`类型的值，我们最终的返回值`Const a c`盒子上的`a`应该是哪一个呢？换句话说，在不知道`a`的具体类型的情况下和其他约束的情况下，我们没办法做出新的盒子。

应用函子
--------

我们把刚刚希望被抽象出来的，能够提供`<*>`定义的函子称为应用函子（Applicative Functor），在Haskell中`Applicative`类型类代表的就是这一类类型：

```
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

我们要求函子是应用函子的父类型类，而如果一个函子想成为应用函子，必须提供两个函数：

+ `pure`接收一个参数并把它包裹到函子里，这个函数的作用就是上面提到的，赋予参数一个不影响计算语义的盒子，我们也常常把这个升格值的过程称作添加最小上下文（minimum context）。

+ `<*>`是升格计算的核心，它可以把包裹在函子中的函数和包裹在函子中的参数取出并计算，或者根据函子的上下文直接给出结果，这也是为什么`<*>`被称作函子应用运算符的原因。计算的结果将仍然被包裹在函子类型中。

“添加最小上下文”这件事情成了一个略显模糊的定义，实际上`pure`和`<*>`函数一定需要满足某些条件，否则满足类型的函数太多，我们要求对于任意一个应用函子，`pure`和`<*>`函数必须满足下面四个条件：


+ 单位律（identity）：`pure id <*> v` ≡ `v`

+ 组合律（composition）：`pure (.) <*> u <*> v <*> w` ≡ `u <*> (v <*> w)`

+ 同态律（homomorphism）：`pure f <*> pure x` ≡ `pure (f x)`

+ 互换律（interchange）：`u <*> pure y` ≡ `pure ($ y) <*> u`

这些条件一起共同约束起应用函子的两个函数，让它们不会产生模棱两可的计算语义。而实际上，刚刚举例无法成为应用函子的`Const a`正是因为没有办法提供适合的`pure`和`<*>`，才停留在了函子的分类中，在下一章，你会发现在某些特殊情况下，`Const a`也可能成为应用函子，不过那完全是另外一个故事了。

###Reader应用函子###

之前我们说过Haskell中的函数类型`(->) a`其实可以看作一个函子，`a -> b`类型的函数可以看作被包裹在`(->) a`类型的函子中的`b`类型的值，而相应的`fmap`操作就是函数组合，实际上`(->) a`不仅仅是函子，还是一个应用函子，下面我们就来研究一下`(->) a`是如何成为应用函子的实例的，它又具有什么样的计算语义。

根据应用函子的定义，我们需要提供下面类型的两个函数：

```
pure :: x -> (a -> x)
(<*>) :: (a -> (x -> y)) -> (a -> x) -> (a -> y)

-- 根据->是右结合的，下面的类型是等价的
pure :: x -> a -> x
(<*>) :: (a -> x -> y) -> (a -> x) -> (a -> y)
```

注意`(->) a`就是`a ->`，对于`pure`函数而言，我们接收到了一个`x`类型的参数，需要返回一个`a -> x`类型的函数，鉴于我们不知道`a`和`x`的类型关系，我们能做的，只有生成一个原封不动返回参数的函数：

```
instance Applicative ((->) a) where
    pure x = \_ -> x
    -- point-free
    pure = const
```

而对于`<*>`的类型，我们注意到下面几点：

+ 第一个参数是一个函数，这个函数需要`a`类型参数，返回`x -> y`类型的函数。

+ 第二个参数还是一个函数，这个函数需要`a`类型参数，返回`x`类型的值。

+ 最终需要返回`a -> y`类型的函数。

由于最终返回的函数会接收到`a`类型的参数，我们可以先把它交给第一个函数，得到`x -> y`类型的函数，再交给第二个函数，得到`x`类型的值，最后把这个值交给`x -> y`类型的函数，从而得到`y`类型的结果：

```
(<*>) :: (a -> (x -> y)) -> (a -> x) -> (a -> y)
fxy <*> fx = \a -> fxy a $ fx a
-- (<*>) f g x = f x (g x)

hyperSum = pure (\x y z -> x + y + z) <*> (^2) <*> (^3) <*> (^4)
-- \x -> (x^2) + (x^3) + (x^4)

hyperSum 3
-- 117
```

我们看到这个函子应用运算符一定会返回`a -> ...`类型的函数，其中`a`类型的参数被从始至终贯穿整个运算，传给了每个被连接的`a -> ...`类型的函数，这些函数的返回值被当成参数传递给初始创建应用函子时的计算，这有点像一个全局的绑定，不同的是我们并没有手动的把这个绑定单独传递给每一个参与运算的函数，`<*>`函数已经帮助我们完成了背后的穿针引线。

这个应用函子常常被用在配置模块化的问题上，程序运行需要的配置数据就是函子中`a`的类型，其他需要读取配置的函数类型一定都是`a -> ...`，而如果我们要组合若干个需要读取配置的函数，通过`<*>`把它们连接起来就可以得到一个新的`a -> ...`类型的函数，给这个函数传递一个配置，就相当于给运算中包含的所有需要配置的函数传递了相同的配置，而作为这些子函数的作者，就可以不用关心具体从哪里取得配置了，只需要书写相应的`a -> ...`类型的函数即可。

`(->) a`这个函子也被称为读取（Reader）函子，因为这个函子会把需要读取`a`的函数组合起来，生成一个大的读取函数，`a`类型的参数就是读取函子要读取的目标。回到`(->) a`本身的含义，即一个需要读取`a`类型参数的函数，`<*>`提供的是一个组合这类函数的方法。

###Applicative Style###

自然升格（Applicative Style）这个词语指的是使用应用函子构建运算的一种书写习惯，我们先来看一个辅助函数：

```
(<$>) :: (a -> b) -> -> f a -> f b
f <$> x = fmap f x
-- <$> = fmap

infixl 4 <$>

(+1) <$> [1,2,3]
-- [2,3,4]
```

`<$>`其实就是函数`fmap`的中缀版本，和`$`不同的是`<$>`是一个左结合的中缀函数，在自然升格里的作用是把一个函数先升格至函子的范畴，然后就可以方便的使用`<*>`去继续应用计算：

```bash
Prelude> (+) <$> Just 1 <*> Just 2
Just 3
Prelude> (+) <$> Just 1 <*> pure 2
Just 3
Prelude> replicate <$> Just 10 <*> Just 'x'
Just "xxxxxxxxxx"
Prelude> replicate <$> Nothing  <*> Just 'x'
Nothing
Prelude> replicate <$> [1,2,3] <*> ['x', 'y', 'z']
["x","y","z","xx","yy","zz","xxx","yyy","zzz"]
Prelude> (\x y z -> x + y + z) <$> (^2)  <*> (^3) <*> (^4) $ 3
117
```

编译器自动根据参数类型推导出了我们需要的应用函子类型，形如：

```
... <$> ... <*> ... <*> ...
```

这类写法就叫自然升格，这个写法的第一个表达式是个参数数量为n的函数，后面用`<$>`连接第一个参数，得到升格之后的后续运算，然后使用`<*>`连接剩下的n-1个参数即可，升格的过程在第一个`<$>`中被自然完成了，需要注意这些参数都需要包裹在函子类型里。

有时我们希望在计算的过程中直接填充函子类型，模块Data.Functor中提供了在自然升格写法下需要的两个中缀函数：

```
(<$) :: Functor f => a -> f b -> f a
(<$) = fmap . const

infixl 4 <$

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

infixl 4 $>


[1..10] $> 'a'
-- "aaaaaaaaaa"

3 <$ Just 'x'
-- Just 3

(\x y z -> x + y + z) <$> (^2) $> 10 $ 3
-- 10
```

上面最后一个例子中，我们直接使用`10`填充了`a -> ...`类型的函子盒子，得到了一个相当于`const 10`的读取函数，于是最后不管你传递什么参数，结果都是`10`。

值得注意的是，这两个函数中我们并不要求`f`一定是应用函子，因为定义中我们只用到了函子实例的`fmap`，而函子是应用函子的父类型类，所以在自然升格的过程中也可以使用它们。

还有的时候，我们希望直接使用某个包裹在函子的值填充到生成的函子中，在Control.Applicative中还定义了如下两个中缀函数：

```
(*>) :: Applicative f => f a -> f b -> f b
a1 *> a2 = (id <$ a1) <*> a2

(<*) :: Applicative f => f a -> f b -> f a
(<*) = flip (*>)

infixl 4 <*, *>

(\x y z -> x + y + z) <$> (^2) <*> (^2)  *> (+10) $ 3
-- 13

replicate <$> Just 1 *> Just (+1) <*> Just 1234
-- Just 1235

replicate <$> Just 2 <* Just (+1) <*> Just 1234
-- Just [1234,1234]
```

在自然升格的写法中，所有运算符的优先级都是4，结合性都是从左向右，`<*`、`*>`是用来舍弃右侧或者左侧未完成的包裹在函子中的计算或值，而`<$`、`$>`是用来填充左侧或者右侧的函子的。这些函数理解上去有些烧脑，但是仔细沿着函子和应用函子的定义，就不难弄清楚了，关于它们的应用，在后面第三部分里深入研究解析器（parser）的时候会再介绍。

需要注意的是，很多新手会误认为`(<*) :: f a -> f b -> f a`和`const`函数一样做的事情一样，就是直接忽略第二个参数，这其实是不对的，`<*`和`const`的类型`a -> b -> a`的区别就在`f`这个应用函子携带的上下文信息上：

```
Nothing `const` Just 3
-- Nothing

Just 3 `const` Nothing
-- Just 3

Nothing <* Just 3
-- Nothing

Just 3 <* Nothing
-- Nothing
```

通过```Just 3 `const` Nothing```和`Just 3 <* Nothing`的区别就可以看出来，`<*`做的不仅仅是丢弃右侧的参数，而是根据左右两侧函子携带的上下文信息，把左侧函子包裹的值重新打包，这个过程中，右侧的参数中函子携带的信息将会影响到最终的结果。类似的，我们还有：

```
Prelude Data.Functor Control.Applicative> [1..2] <* [1..10]
[1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2]
```

这里虽然结果中只包含左侧函子包裹的值，但是右侧函子盒子的形状最终决定了结果中函子盒子的形状。

最后再补充介绍下和自然升格相对应的显式升格的写法，回顾本文开篇的一个例子：

```
liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe2 f (Just x) (Just y) = Just $ f x y
liftMaybe2 _ Nothing _ = Nothing
liftMaybe2 _ _ Nothing = Nothing
```

我们同样可以把这种针对两个参数的函数的升格操作扩展到全部的应用函子上：

```
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = fmap f x <*> y

liftA2 replicate (Just 3) (Just 'x')
-- Just "xxx"
```

在Control.Applicative模块中定义了参数数量从1到3的常用升格函数：

```
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```

不难看出其实`liftA`就是`fmap`，在参数数量继续增加的情况下，显式升格并不会带来可读性上的增加，反而导致难以书写，所以标准库中也没有定义参数更多的情况。而且一旦理解了包裹在函子中的计算这个概念，自然升格的写法往往也非常符合直觉，而`liftAx`系列函数，更多的是在定义其它控制函数的时候被用到。

IO 应用函子
----------

之前说过，`IO`是一个函子类型，我们可以使用`fmap`把函数作用在被函子包裹的值，而这个函子包裹的是和系统的输入输出相关的值，例如用户的输入，或是读取磁盘上的文件，再或是从网络端口读取的数据，实际上这个函子的作用就是保证和外界的交互，这个函子同样也是一个应用函子，例如下面这个例子：

```bash
Prelude Control.Applicative> liftA2 (&&) readLn readLn
False
False
False
```

`readLn :: Read a => IO a`会返回一个包裹在`IO`函子类型中的可以反序列化的值，这里由于`&&`的类型是`Bool -> Bool -> Bool`，所以`readLn`返回的值是`IO Bool`，`liftA2 (&&)`得到了一个`IO Bool -> IO Bool -> IO Bool`的函数，由于两个参数都被包裹在了`IO`中，即使`&&`在遇到第一个`False`之后已经不需要第二个判断参数了，但是读取的这个过程仍然也会发生，因为这是`IO`函子带来的副作用，我们比较下`Maybe`函子：

```
Prelude Control.Applicative> liftA2 (&&) (Just False) (Just undefined)
Just False
```

即使包裹在第二个参数中的值是底，在`&&`接收到第一个参数是包裹在`Maybe`函子中的`False`之后，就不会在求取第二个函子里的值了，而如果是`IO`函子的话，第二次读取外界参数的过程并不会消失，这并不是因为我们需要知道第二个函子中的值，而是需要第二个函子包裹本身，在后面讲到`IO`单子的时候，我们再解释这个`IO`类型的包裹到底是什么。
