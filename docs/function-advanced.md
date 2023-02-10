# 函数 - 进阶

### 模式匹配

模式匹配通过`检查数据的特定结构`来判定其是否匹配，并按模式从中取得数据。

#### 函数

定义函数时，可以为不同的模式分别定义函数体：

```haskell
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
```

调用`lucky`时，模式会从上至下匹配，一旦有匹配，对应的函数体就会应用。`lucky x`是一个万能匹配模式，如果把它移到`lucky 7`前，就永远不会匹配到`lucky 7`。

未匹配到对应的模式，匹配会失败：

```haskell
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- ghci> charName 'h'
-- "*** Exception: test.hs:(2,1)-(4,22): Non-exhaustive patterns in function charName
```

> 函数使用匹配模式时，留一个万能匹配模式，程序就不会因为不可预料的输入而崩溃了。

#### 元组

Tuple 可以通过模式匹配取出对应的元素。

```haskell
first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
second (_, _, c) = c
```

> `_`是一个占位符，表示不关心这部分的具体内容。

Tuple 在列表推导中也可以使用模式匹配：

```shell
ghci> let xs = [(1, 3), (4, 3), (2, 4)]
ghci> [a + b | (a, b) <- xs]
[4,7,6]
```

#### 列表

List 也可以使用模式匹配，可以用`[]`或者`:`来匹配它。

`x:xs`这样的模式可以将 List 的头部绑定为`x`，尾部绑定为`xs`。如果只有一个元素，那么`xs`就是一个空 List。

```haskell
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element:" ++ show x
tell (x:y:[]) = "The list has two elements:" ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long. The first two elements are: " ++ show x ++ " and " ++ show y

ghci> tell [1, 2]
"The list is long. The first two elements are: 1 and 2"
```

`(x:[])`与`(x:y:[])`也可以写作`[x]`和`[x,y]`，但`(x:y:_)`这样的模式就不行，因为它匹配的 List 长度不固定。

```haskell
tell [x] = "The list has one element:" ++ show x
tell [x,y] = "The list has two elements:" ++ show x ++ " and " ++ show y
```

##### as 模式

将一个`标识符`和`@`置于模式前，就能在按模式分割的同时保留对整体的引用。

```haskell
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

ghci> capital "Dracula"
"The first letter of Dracula is D"
```

### 守卫

守卫由跟在函数名及参数后面的`|`标志，通常它们是靠右一个缩进排成一列。

一个守卫就是一个布尔表达式，如果它的计算结果为`True`，则使用相应的函数体。如果为`False`，则检查会传递给下一个守卫。

```haskell
densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
	| mass / volume < 1.2 = "Wow! You're going for a ride in the sky!"
	| mass / volume <= 1000.0 = "Have fun swimming, but watch out for sharks!"
	| otherwise = "If it's sink or swim, you're going to sink."
```

最后一个守卫往往都是`otherwise`，它的定义就是简单一个`otherwise = True`，捕获一切。

守卫和模式匹配能很好的结合起来，如果没有找到合适的守卫，就会转入到下一模式：

```haskell
dayTell :: Int -> String
dayTell day
	| day == 1 = "Monday"
	| day == 2 = "Tuesday"

deyTell day
	| day == 3 = "Wednesday"
	| otherwise = "Not matching."
```

守卫可以只写在一行，但是会丧失可读性：

```haskell
max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b
```

### Where

`where`绑定可以在函数中定义重复使用的函数或名字。

```haskell
calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density m v | (m, v) <- xs]
	where density = mass volume = mass / volume
```

配合守卫使用时，`where`的定义应尽量写在守卫之后，这样对所有的守卫都是可见的：

```haskell
densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
	| density < air = "Wow! You're going for a ride in the sky!"
	| density / water <= 1000.0 = "Have fun swimming, but watch out for sharks!"
	| otherwise = "If it's sink or swim, you're going to sink."
	where density = mass / volume
	      air = 1.2
              water = 1000.0
```

> `where`定义的函数或名字，仅能在当前模式中使用，并且所有函数或名字都需要在单个列上对齐，否则编译器无法正确识别。

`where`列表模式匹配语法糖：

```haskell
initials :: String -> String -> String
initials firstname lastname = [f] ++ '. ' ++ [l] ++ '.'
	where (f:_) = firstname
              (l:_) = lastname
```

`where`函数模式匹配：

```haskell
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
	where what [] = "empty"
              what [x] = "a singleton list."
              what xs = "a longer list."
```

### Let

`let`绑定是一个表达式，能够在任何位置定义局部函数或名字。

`let`的格式为`let [bindings] in [expressions]`，在`let`中的绑定仅对`in`部分可见。

```haskell
ghci> 4 * (let a = 9 in a + 1) + 2
42
```

`let`定义局部函数：

```haskell
ghci> [let square x = x * x in (square 5, square 3, square 2)]
[(25,9,4)]
```

多个名字可以绑定在多行：

```haskell
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let sideArea = 2 * pi * r * h
            topArea = pi * r ^ 2
	in  sideArea + 2 * topArea
```

也可以在一行，最后一个绑定后面的分号是可选的：

```haskell
ghci> [let a = 100; b = 200; c = 300 in a * b * c]
[6000000]
```

`let`绑定中使用模式匹配：

```haskell
ghci> (let (a, b, c) = (1, 2, 3) in a + b + c) * 100
600
```

`let`绑定在列表推导中使用：

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
```

列表推导中`let`的绑定只在输出函数（`|`之前的部分）和限制条件中可见，因此可以省略掉`in`部分。

`let ... in`也放在限制条件或输出函数中，这样名字只对当前限制条件或输出函数可见：

```haskell
ghci> [let a = 2 in a + x | x <- [1, 2, 3]]
[3, 4, 5]
```

GHCi 中`in`部分也可以省略，名字在整个交互中可见：

```haskell
ghci> let zoot x y z = x * y + z
ghci> zoot 3 9 2
29
```

> `let`和`where`对区别在于，`let`是一个表达式，而`where`是个语法结构。

### Case

Haskell 中的`case`是一个表达式，可以对变量的不同情况分别求值。

`case`表达式语法：

```haskell
case expression of pattern -> result
	           pattern -> result
                   ...
```

模式匹配本质上是`case`的语法糖，以下两段代码完全等价：

```haskell
head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x:_) = x


head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x
```

`expression`匹配合适的模式。一如预期的，第一个模式若匹配，就执行第一个区块的代码，否则就比对下一个模式，如果最后没有匹配的模式，就会抛出错误。

函数参数的模式匹配只能在定义函数时使用，而`case`表达式可以用在任何地方：

```haskell
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."																	       [x] -> "a singleton list."
                                               xs -> "a longer list."
```
