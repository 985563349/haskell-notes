# 高阶函数

高阶函数是指能接收一个或多个函数作为输入，或者将函数作为输出的函数。

### 柯里化

柯里化（Currying），是指把接收多个参数的函数变换成一个接收单一参数的函数，并且返回接收余下参数而且返回结果的新函数的技术。

本质上，Haskell 的所有函数都只有一个参数，所有多个参数的函数都是柯里函数。

例如，`max`函数，以下两种调用方式完全等价：

```shell
ghci> max 4 5
5
ghci> (max 4) 5
5
```

当执行`max 4 5`时，它会返回一个函数，该函数接收一个参数并返回`4`或者该参数，具体取决于谁更大。

检查`max`函数的类型：

```shell
ghci> :t max
max :: Ord a => a -> a -> a
```

函数类型中的`->`仅用于分割参数和返回值的类型。

由此可以理解`max`函数取一个参数，并返回一个函数，这个函数再取一个参数，完成计算并返回最终结果。

#### 动机

柯里化是一种处理函数中附有多个参数的方法，并在只允许单一参数的框架中使用这些函数。例如，一些分析技术只能用于具有单一参数的函数。现实中的函数往往有更多的参数。

一些编程语言几乎总是使用柯里函数来实现多个参数，例如 Haskell。这个属性是从`lambda演算`继承而来的，其中多参数的函数通常以柯里形式表示。

#### 好处

**不全调用**

若以不全参数来调用，就可以得到一个不全调用的函数，这种方法可以很方便的构造新的函数。

```haskell
multThree :: Num a => a -> a -> a -> a
multThree x y z = x * y * z

-- 构造新函数
ghci> let multTwoWithNine = multThree 9
ghci> multTwoWithNine 2 3
54
ghci> let multWithEighteen = multTwoWithNine 2
ghci> multWithEighteen 10
180
```

中缀函数也可以不全调用，只需要用括号把它和一边的参数括在一起即可：

```haskell
divideByTen :: Floating a => a -> a
divideByTen = (/10)
ghci> divideByTen 200  -- 等价于：(/10) 200、200 / 10
20

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])
ghci> isUpperAlphanum 'a' -- 等价于：'a' `elem` ['A' .. 'Z']
False
```

> `-`运算符是唯一的例外，无法做不全调用。处于计算上的方便，如`(-4)`表示负`4`。如果需要减法的不全调用可以使用`subtract`函数。

### Point Free

Point Free 是一种编程风格，指函数定义不标识所要操作的参数，只是和其他函数的复合。也可以理解为，不使用要处理的值，只合成运算过程。

例如：

```haskell
addOne x = x + 1
square x = x * x

-- 组合
addOneThenSquare = addOne . square
ghci> addOneThenSquare 2
5
```

`addOneThenSquare`是一个合成函数，在定义它时，根本不需要提到要处理的值，这就是 Point Free。

Point Free 的本质就是使用一些通用的函数，组合出各种复杂的运算。上层运算不直接操作数据，而是通过底层函数去处理。

它的好处是，运算过程抽象化，处理一个值，但是不提到这个值。能让代码更加清晰简练，更符合语意，更容易复用、测试。但需要注意的是，用好 Point Free 最先要做的，是为函数起一个通俗易懂的名字，通过名字就能知道函数的功能不用去深入函数细节。

### 偏函数

偏函数（Partial Application），也叫`部分应用`。是指固定多参数函数的部分参数，并返回一个可以接受剩余部分参数的函数的转换过程。

它的好处是，能减少重复传参，提高函数的适用性，以及固定执行环境的上下文。

#### 偏函数与柯里化的区别

偏函数是通过固定一部分参数，生成一个参数数量更少的函数。

柯里化是将一个多参数函数，变换成一个单参数的函数链。
