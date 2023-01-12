# 元组

元组（Tuple）很像 List，都是将多个值存入一个容器，但却又有着本质的不同。List 内的元素类型必须相同且不关心元素的数量，而 Tuple 不要求元素的类型相同，但数量必须明确。

使用 Tuple 前应当明确数据中应该有多少项，每个长度不同的元组都是不同的类型，一旦定义就不能再追加元素。

### 定义元组

Tuple 中的元素由括号括起，并由逗号隔开：

```shell
ghci> ("Jack", 22)
("Jack",22)
ghci> (1, 2, 3)
(1,2,3)
```

声明单个元素的 Tuple 没有意义：

```shell
ghci> (1)
1
```

> 只有两个元素的 Tuple 称为序对（pair），三个元素的称为三元组，四个元素的称为四元组......

### 比较元组

Tuple 中存有可比较元素，且类型相同时就可以进行比较，比较规则与 List 相同。

```shell
ghci> (1, 2, 3) > (1, 0, 1)
True
ghci> (1, 2, 3) > (1, 3, 1)
False
```

长度不一致，会被认为类型不同无法进行比较：

```shell
ghci> (1,2,3) > (1,2)

<interactive>:14:11: error:
    • Couldn't match expected type: (a0, b0, c0)
                  with actual type: (a1, b1)
    • In the second argument of ‘(>)’, namely ‘(1, 2)’
      In the expression: (1, 2, 3) > (1, 2)
      In an equation for ‘it’: it = (1, 2, 3) > (1, 2)
```

### 常用函数

**fst**：返回序对的首项（非序对会报错）

```shell
ghci> fst (8, 11)
8
```

**snd**：返回序对的尾项（非序对会报错）

```shell
ghci> snd (8, 11)
11
```

**zip**：根据传入的 List 生成序对

```shell
ghci> zip [1, 2, 3] [1, 2, 3]
[(1, 1), (2, 2), (3, 3)]
ghci> zip [1, 2, 3, 4] [1, 2, 3]
[(1, 1), (2, 2), (3, 3)]
```
