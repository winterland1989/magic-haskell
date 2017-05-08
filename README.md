魔力Haskell官方网站
===================

欢迎来到[《魔力Haskell》的官方网站](http://magic-haskell.com/)，这里提供书中的[样章](https://github.com/winterland1989/magic-haskell/tree/gh-pages/demo)和[范例](https://github.com/winterland1989/magic-haskell/tree/gh-pages/code)。此外还有本人正在录制的一套[Haskell系列教程](http://www.iqiyi.com/w_19ruf9y619.html)的[配套slide](https://github.com/winterland1989/magic-haskell/tree/gh-pages/haskell%E7%B3%BB%E5%88%97%E6%95%99%E7%A8%8B%40didiFP)。

# 用cabal运行

运行范例需要电脑上提前安装`cabal-install`和`ghc`，你可以从[这里](https://www.haskell.org/downloads)获得它们，之后只需要：

```bash
git clone https://github.com/winterland1989/magic-haskell.git
cd ./magic-haskell/code
cabal sandbox init
cabal install --only-dependencies
cabal build
```

即可编译，执行如下命令运行范例：

```bash
# 第6章 常用的列表操作 最小子串和问题
cabal run -- min-sub-list

# 第12章 透镜组
cabal run -- lens

# 第15章 解析器
cabal run -- arg-parser

# 第17章 八皇后问题和列表单子
cabal run -- queens

# 第18章 Reader单子
cabal run -- reader-render

# 第19章 State单子
cabal run -- state-calculator

# 第24章 单子变换 猜数字游戏
cabal run -- guess-number

# 第25章 单子变换的升格操作
cabal run -- calculator

# 第28章 数据库操作
cabal run -- database

# 第29章 模版编程 (相关代码: Template.hs, MakeLenses.hs)
cabal run -- template

# 第31章 高级类型编程 异类列表
cabal run -- hlist
```

# 用stack运行
`stack`和`cabal sandbox`类似，也是创建一个独立的环境。 安装见 https://docs.haskellstack.org/en/stable/README/ 。

```bash
cd code && stack build

# 第6章 常用的列表操作 最小子串和问题
stack MinSubList.hs

...
```

另外对于本书有任何建议，请直接在本项目上提交issue，我会尽快回复并记录。感谢各位读者的支持，希望这本书能在大家学习、工作的路上有所帮助。
