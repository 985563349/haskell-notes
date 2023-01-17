# 类型和类型类

Haskell 是静态类型的，在编译时每个表达式的类型都已明确，这就提高了代码的安全性。

Haskell 支持类型推导，如定义一个数字，编译器能自动识别出来，因此不必在每个函数或表达式上标明类型。

### 检测类型

GHCi 中可以使用`:t`命令来检测表达式的类型：

```shell
ghci> :t 'a'
'a' :: Char
ghci> :t True
True :: Bool
ghci> :t "HELLO!"
"HELLO!" :: String
ghci> :t (True, 'a')
(True, 'a') :: (Bool, Char)
ghci> :t 4 == 5
4 == 5 :: Bool
```

`:t`命令处理一个表达式的输出结果为表达式后跟`::`及其类型，凡是明确的类型，其首字母必须大写。

检测函数的类型：

```shell
ghci> let removeNonUppercase xs = [x | x <- xs, x `elem` ['A'..'Z']]
ghci> :t removeNonUppercase
removeNonUppercase :: [Char] -> [Char]
```

`removeNonUppercase`的类型为`[Char] -> [Char]`，参数类型与返回值类型之间是由`->`分隔。从参数和返回值中可以看出，它将一个字符串映射成另一个字符串。

### 定义类型

常量：

```haskell
text :: String
text = "HELLO!"
```

函数：

```haskell
removeNonUppercase :: String -> String
removeNonUppercase xs = [x | x <- xs, x `elem` ['A'..'Z']]
```

> `[Char]`与`String`是等价的，但是使用`String`会更清晰。

多参数函数：

```haskell
addTree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
```

### 常见类型

**Int**：整型，大小是有上下界的。

**Integer**：大整型，大小是无上下界的，效率不如 Int 高。

**Float**：单精度浮点型

**Double**：双精度浮点型

**Bool**：布尔型

**Char**：字符型

**String**：字符串型

**Ordering**：值只能是`GT`、`LT`和`EQ`的类型，分别表示大于、小于和等于

**Tuple 类型**：Tuple 类型取决于它的长度及其中间项的类型，空 Tuple 也是个类型，它只有一种值`()`

### 类型变量

使用`:t`检测`head`函数，返回类型中的`a`就是类型变量：

```shell
ghci> :t head
head :: [a] -> a
```

类型变量代表可以是任意的类型，这一点和其他语言中的`泛型`很相似，使用到类型变量的函数被称作`多态函数`。

在命名上，类型变量可以使用多个字符，但通常都是使用单个字符，如`a,b,c,d...`。

### 类型类

类型定义行为的接口，如果一个类型属于某类型类，那它必实现了该类型类所描述的行为。类型类和`接口（interface）`类似，可用于约束变量类型。

约束变量类型必须在`Eq`类型类中：

```shell
ghci> :t (==)
(==) :: Eq a => a -> a -> Bool
```

> `=>`符号左侧部分就是`类型约束`。

多个类型约束：

```shell
ghci> :t fromIntegral
fromIntegral :: (Integral a, Num b) => a -> b
```

### 常见类型类

一个类型可以是多个类型类的实例，一个类型类可以有多个类型实例。

**Eq**：可判断相等性的类型

```shell
ghci> :t (==)
(==) :: Eq a => a -> a -> Bool

ghci> :t (/=)
(/=) :: Eq a => a -> a -> Bool

ghci> 5 == 5
True
ghci> 5 /= 5
False
```

除了函数类型，其他类型都是`Eq`的实例。

**Ord**：可比较大小的类型

```shell
ghci> :t compare
compare :: Ord a => a -> a -> Ordering

ghci> compare 1 2
LT

ghci> 5 >= 2
True

ghci> "ac" > "bd"
False
```

除了函数类型，其他类型都是`Ord`的实例。

**Show**：可用字符串表示的类型

```shell
ghci> :t show
show :: Show a => a -> String

ghci> show True
"True"
```

除了函数类型，其他类型都是`Show`的实例。

**Read**：能解析字符串表示，产生值的类型

```shell
ghci> :t read
read :: Read a => String -> a

ghci> read "True" || False
True
ghci> read "8.2" + 3.8
12.0

# 无法通过上下文确定类型
ghci> read "4"
*** Exception: Prelude.read: no parse

# 通过 :: 类型注释确定具体类型
ghci> read "4" :: Int
4
```

除了函数类型，其他类型都是`Read`的实例。

**Enum**：枚举类型

```shell
ghci> ['a' .. 'e']
"abcde"
ghci> [LT .. GT]
[LT,EQ,GT]
ghci> [3 .. 5]
[3,4,5]
ghci> succ 'B'
'C'
```

`Enum`类型类要求实例有后继子`(successors)`和前置子`(predecesors)`，分别可以通过`succ`函数和`pred`函数得到。

该类型类的实例有：`Int`、`Integer`、`Float`、`Double`、`Char`、`Bool`、`Ordering`、`()`。

**Bounded**：具有上下界的类型

```shell
ghci> :t minBound
minBound :: Bounded a => a
ghci> :t maxBound
maxBound :: Bounded a => a

ghci> minBound :: Int
-9223372036854775808
ghci> maxBound :: Char
'\1114111'
ghci> maxBound :: Bool
True
ghci> minBound :: Bool
False
```

`Bounded`类型类要求实例有上界和下界，分别可以通过`maxBound`函数和`minBound`函数得到。

如果每一项都属于`Bounded`类型类，那么该 Tuple 也属于`Bounded`。

```shell
ghci> maxBound :: (Bool, Int, Char)
(True,9223372036854775807,'\1114111')
```

该类型类的实例有：`Int`、`Char`、`Bool`。

**Num**：整数与实数类型

```shell
ghci> :t 20
20 :: Num a => a
```

所有的数字都是`多态常量`，可以具有任何`Num`类型类的实例的特征。

该类型类的实例有：`Int`、`Integer`、`Float`、`Double`。

**Integral**：整数类型

该类型类的实例有：`Int`、`Integer`。

**Floating**：浮点类型

该类型类的实例有：`Float`、`Double`。
