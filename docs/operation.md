# 运算

### GHCi

打开终端，输入 `ghci`，显示如下信息表示已成功进入 GHCi：

```shell
GHCi, version 9.2.4: https://www.haskell.org/ghc/  :? for help
ghci>
```

使用`:q`命令可以退出 GHCI：

```shell
ghci> :q
Leaving GHCi.
```

使用快捷键`Ctrl + l`可对 GHCI 快速清屏。

### 四则运算

以下是一些简单的运算：

```shell
ghci> 2 + 15 17
ghci> 1892 - 1472 420
ghci> 49 * 100 4900
ghci> 5 / 2 2.5
```

通过括号可以更改优先级次序：

```shell
ghci> 50 * 100 - 4999
1
ghci> 50 * (100 - 4999)
-244950
```

负数在参与运算时需要添加括号，否则会抛出异常：

```shell
ghci> 5 * -3

<interactive>:7:1: error:
    Precedence parsing error
        cannot mix ‘*’ [infixl 7] and prefix `-' [infixl 6] in the same infix expression

ghci> 5 * (-3)
-15
```

### 逻辑运算

haskell 中使用`True`来表示逻辑真，使用`False`来表示逻辑假。逻辑运算也同样直白，`&&`指逻辑与，`||`指逻辑或，`not`指逻辑非。

```shell
ghci> True && False
False
ghci> True && True
True
ghci> False || True
True
ghci> not False
True
```

### 比较运算

haskell 进行相等性判定时，使用`==`表示相等，使用`/=`表示不等，使用`>`表示大于，使用`<`表示小于，使用`>=`表示大于等于，使用`<=`表示小于等于。

```shell
ghci> 5 == 5
True
ghci> 5 /= 5
Flase
ghci> 3 > 4
False
ghci> 5 < 6
True
ghci> 1 >= 1
True
ghci 2 <= 3
False
```

haskell 中`+`运算符要求两端都是数值，`==`运算符仅对两个可比较的值可用，这就要就它们的类型都必须一致。

```shell
ghci> 5 + '1'

<interactive>:1:3: error:
    • No instance for (Num Char) arising from a use of ‘+’
    • In the expression: 5 + '1'
      In an equation for ‘it’: it = 5 + '1'

ghci> "hello" == "hello"
True
```

`整数` + `浮点数`是可以执行的，整数会被看作浮点数，但浮点数不能被看作整数。

```shell
ghci: 5 + 4.0
9.0
```
