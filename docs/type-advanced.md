# 类型和类型类 - 进阶

### 定义类型

使用`data`关键字可以构造自定义类型，语法为：`data 类型名 = 值构造器 | 值构造器`。需要注意的是，类型名与值构造器的首字母必须大写。

标准库中`Bool`类型的定义：

```haskell
data Bool = False | True
```

自定义类型`Shape`：

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

当一个类型仅有唯一值构造器时，类型名和值构造器名可以重复。

```haskell
data Point = Point Float Float
```

#### 导出类型

模块内导出数据类型与导出函数的方式相同。如果要导出值构造器就在类型名后添加`()`，并列出要导出的值构造器，多个使用逗号隔开。如果要导出所有值构造器，只需要写`(..)`即可。

```haskell
module Shapes {
  Point(..),
  Shape(Circle),
  area
} where
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

通过记录语法创建类型数据时不用关心各字段的顺序。

```shell
ghci> let point = Point { y = 1.0, x = 2.2 }
ghci> point
Point {x = 2.2, y = 1.0}
ghci> x point
2.2
```

记录语法适用于构造器的字段较多且不易区分的情况，如果字段够直白，使用构造器会更简洁。

### 类型参数

值构造器可以取几个参数，产生一个新值。与之相似，类型构造器可以取类型作为参数，产生新的类型。

例如，标准库中的`Maybe`类型：

```haskell
date Maybe a = Nothing | Just a
```

类型定义中的`a`就是一个类型参数，因为有了类型参数，Maybe 就成为了一个类型构造器。

单纯的 Maybe 类型的值不存在，因为 Maybe 不是类型，而是类型构造器。要成为真正的类型，需要传递类型参数。例如，`Maybe Int`、`Maybe String`等诸多类型。

因为 Haskell 支持类型推导，所以很少需要显式地为类型构造器传递类型参数。只要写入值`Just 'a'`，Haskell 就能自动推导出它是`Maybe Char`类型。

如果想要显式的传递类型参数，就需要对类型部分做修改。

例如，`Just 3`会默认被推导为`Num a => Maybe a`，如果期望的类型是`Maybe Int`，就可以使用类型注解进行约束。

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

> 在一个类型派生为 Eq 的实例后，就可以进行相等判定。Haskell 会首先检查两个值的值构造器是否一致（这里只有单值构造器），再用 == 来检查其中每一对字段的数据是否相等。唯一的要求是：其中所有字段的类型都必须属于 Eq 类型类。

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

所谓的代数数据类型，指的是多个类型组合成新的类型的方式，这些组合方式拥有一些代数特征（可以理解为和或积）。

### 类型别名

使用`type`关键字可以定义类型别名。

```haskell
type String = [Char]
```

类型别名什么都没做，只是提供了一个不同的名字。

类型别名也可以使用参数。

```haskell
type IntMap = Map int
-- 等价于
type IntMap v = Map Int v
```
