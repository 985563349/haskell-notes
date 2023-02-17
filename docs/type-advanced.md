# 类型和类型类 - 进阶

### 定义类型

类型使用`data`关键字来定义，语法为：`data 类型名 = 值构造器 | 值构造器`。需要注意的是，类型名与值构造器的首字母必须大写。

标准库中 Bool 类型的定义：

```haskell
data Bool = False | True
```

自定义类型 Shape：

```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

值构造器本质上是一个返回数据类型值的函数。

```shell
ghci> :t Circle
Circle :: Float -> Float -> Float -> Shape
ghci> Circle 10 20 10
Circle 10.0 20.0 10.0
```

值构造器可以进行模式匹配。

```haskell
area :: Shape -> Float
area (Circle _ _ r) = pi * r * 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

ghci> area $ Circle 10 20 10
314.15927
ghci> area $ Rectangle 0 0 100 100
10000.0
```

> 模式匹配中匹配的`[]`、`5`、`False`等，本质上都是不带参数的值构造器。

当一个类型仅有唯一值构造器时，值构造器名和类型名可以重复。

```haskell
data Point = Point Float Float
```

#### 导出类型

模块内导出数据类型与导出函数的方式相同。如果要导出值构造器就在类型名后加`()`，并列出要导出的值构造器，多个使用逗号隔开。如果要导出所有值构造器，只需要写`(..)`即可。

```haskell
module Shapes (
  Point(..),
  Shape(Circle),
  area
) where
```

### 记录语法

Haskell 提供了一种便捷的编写数据类型的方式，即记录语法（record syntax）。

通过记录语法创建的数据类型，会自动生成属性的 get 函数。

```haskell
data Point = Point {
  x :: Float,
  y :: Float,
} deriving (Show)

ghci> :t x
x :: Point -> Float
```

并且不需要关心各字段的顺序。

```shell
ghci> let point = Point { y = 1.0, x = 2.2 }
ghci> point
Point {x = 2.2, y = 1.0}
ghci> x point
2.2
```

记录语法适用于构造器的字段较多且不易区分的情况，如果字段够直白，使用构造器会更简洁。

### 类型参数

值构造器可以取参数，产生新值。与之相似，类型构造器可以取类型作为参数，产生新的类型。

例如，标准库中的 Maybe 类型：

```haskell
date Maybe a = Nothing | Just a
```

类型定义中的`a`就是一个类型参数，因为有了类型参数，Maybe 就成为了一个类型构造器。

单纯的 Maybe 类型的值不存在，因为 Maybe 不是类型，而是类型构造器。要成为真正的类型，需要传递类型参数。例如，`Maybe Int`、`Maybe String`等类型。

Haskell 支持类型推导，所以很少需要显式地为类型构造器传递类型参数。只要写入值`Just 'a'`，Haskell 就能自动推导出它是`Maybe Char`类型。

如果想要显式的传递类型参数，就要对类型部分做修改。例如，`Just 3`会默认被推导为`Num a => Maybe a`，如果期望的类型是`Maybe Int`，就使用类型注解进行约束。

```shell
ghci> Just 3 :: Maybe Int
```

列表`[]`实际上是一个类型构造器，`[Int]`和`[[String]]`存在，但是不存在类型`[]`。

#### 类型约束

类型支持类约束，但是永远不要用。因为即使约束了，在函数中仍然要添加约束。

```haskell
data (Ord k) = Map k v...
```

### 派生实例

使用`deriving`关键字，可以让类型成为指定类型类的实例。

例如：

```haskell
data Point = Point Float Float deriving (Show)
```

当类型成为`Show`类型类的实例后，就可以输出字符串表示了。

多个类型类使用逗号隔开：

```haskell
data Point = Point Float Float deriving (Show, Eq)
```

> 当一个类型派生为 Eq 的实例后，就可以进行相等判定。Haskell 会首先检查两个值的值构造器是否一致（这里只有单值构造器），再用 == 来检查其中每一对字段的数据是否相等。唯一的要求是：其中所有字段的类型都必须属于 Eq 类型类。

#### Enum 类型类

使用代数数据类型能轻易实现枚举，但使用`Enum`类型类与`Bounded`类型类会更好。

```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)
```

取后继：

```shell
ghci> succ Monday
Tuesday
```

取上界：

```shell
ghci> maxBound :: Day
Sunday
```

比大小：

```shell
ghci> Tuesday < Wednesday
True
```

#### 代数数据类型

所谓的代数数据类型，指的是多个类型组合成新的类型的方式，这些组合方式拥有一些代数特征。

### 类型别名

使用`type`关键字可以定义类型别名。

```haskell
type String = [Char]
```

类型别名什么都没做，只是提供了一个不同的名字。

类型别名可以使用参数。

```haskell
type IntMap v = Map Int v
```

类型构造器也支持柯里化。

```haskell
type IntMap = Map Int
```

### Either 类型

Either 类型有两个值构造器，可以用来封装可能为两种类型的值。

标准库中的 Either 类型：

```haskell
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

Maybe 和 Either 最常用于表示可能失败的计算。Maybe 通常会在失败时返回`Nothing`，但有时并不好用，因为`Nothing`中的信息太少，如果希望知道失败的详细原因，使用 Either 更好。

使用 Either 时，错误一律用`Left`构造器，正确结果一律用`Right`构造器。

### 递归数据结构

递归数据结构是指，类型值中包含类型自身。

例如，自定义一个 List 类型：

```haskell
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
```

列表可能是一个空列表，也可能是一个值与另一个列表（可能为空）结合后的结果。

Cons 值构造器类似于列表中`:`的作用，`:`本身也是一个值构造器，会取一个值和另一个列表做参数，返回一个新的列表。

```shell
ghci> 5 `Cons` Empty
Cons 5 Empty
ghci> 4 `Cons` (5 `Cons` Empty)
Cons 4 (Cons 5 Empty)
```

#### 中缀值构造器

以特殊字符命名的函数，都会被视为中缀函数。值构造器本质上也是函数，因而也能利用这一特性。但存在一项限制：中缀值构造器必须以冒号开头。

```haskell
-- 固定性声明（fixity declaration）
infixr 5 :-:
data Lint a = Empty | :-: (List a) deriving (Show, Read, Eq, Ord)
```

将一个函数定义为运算符时，可以为其指定一个固定性规则（fixity）(非必须)，规定运算符的优先级及结合性。

比如，`*`运算符的固定性规则为`infixr 7 *`，`+`运算符的固定性规则为`infixr 6 +`。优先级相同意味着都是左结合（即`4 * 3 * 2` 等同于 `(4 * 3) * 2`），优先级不同时优先级高的先运算（即`5 * 4 + 3` 等同于 `(5 * 4) + 3`）。

使用`a :-: (List a)`替换`Cons a (List a)`：

```shell
ghci> 5 :-: Empty
5 :-: Empty
ghci> 4 :-: 5 :-: Empty
4 :-: (5 :-: Empty)
```

#### 列表相加

标准库中`++`运算的定义：

```haskell
infixr 5 ++
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
```

自定义`^++`运算：

```haskell
infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x:-:xs) ^++ ys = x :-: (xs ^++ ys)

ghci> let a = 4 :-: 5 :-: Empty
ghci> let b = 6 :-: 7 :-: Empty
ghci> a ^++ b
4 :-: (5 :-: (6 :-: (7 :-: Empty)))
```

### 类型类

类型类负责定义行为（如判断相等性、比较顺序、枚举等）。拥有某行为的类型，就作为对应的类型类的实例。

行为通过函数或者待实现函数的声明来定义，实例可以调用所属类型类中定义的全部函数。

#### 定义类型类

类型类使用`class`关键字来定义，语法为：`class 类型类名 类型参数 where`。

标准库中 Eq 类型类的定义：

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
```

类型参数用于表示实例类型，命名可以不是单个字母，但必须保持小写。类型类中的函数实现是可选的，但必须提供类型声明。

类型类中定义的函数类型并非函数的实际类型，如`(==) :: a -> a -> Bool`，检测函数`==`的类型，得到的是`(Eq a) => a -> a -> Bool`。

#### 定义类型类实例

将类型转为类型类的实例使用`instance`关键字来定义，语法为：`instance 类型类 类型 where`。

定义一个类型 TrafficLight：

```haskell
data TrafficLight = Red | Yellow | Green
```

转为 Eq 类型类的实例：

```haskell
instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

ghci> Red == Red
True
ghci> Red /= Yellow
True
```

Eq 类型类的声明中，`==`与`/=`互相依赖，因而只需在实例声明中覆盖其中一个函数即可。这种风格被称为类型类的`最小完备定义` —— 为符合类型类的行为，而必须实现的最少函数。

如果 Eq 类型类是这样定义：

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

Haskell 无法得知两个函数的关系，因而不得不提供两个函数的实现才符合最小完备定义。

转为 Show 类型类的实例：

```haskell
instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Red light"
  show Green = "Green light"

ghci> show Red
"Red light"
```

使用`deriving`能直接派生为类型类的实例，但要覆写类型类的行为就要使用`instance`手动编写实例声明。

#### 子类化

类声明中添加类约束，就能将类型类实现为另一个类型类的子类。

例如 Num 子类：

```haskell
class (Eq a) => Num a where
 ...
```

类约束限制了类型 a 必须是 Eq 的实例，即一个类型要实现为 Num 的实例，必先要实现为 Eq 的实例。

#### 带参数的类型实例

只有具体的类型才能作为类型变量。

例如 Maybe 就不能是类型变量，因为它是一个类型构造器。所以不能这样的定义：

```haskell
instance Eq Maybe where
  ...
```

Maybe 可以取一个参数产生具体类型，但针对所有可能的类型参数提供单独的实例并不现实。对此，Haskell 允许保留相应的类型参数作为类型变量。

```haskell
instance (Eq m) => Eq (Maybe m) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False
```

实例声明中的类约束用于约束类型中的内容，如代码中 m 所代表的类型被约束为 Eq 的实例。

#### 检测类型类实例

在 GHCi 中通过`:info`命令可获取当前类型类拥有哪些实例。

```shell
ghci> :info Maybe
```
