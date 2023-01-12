# 列表

Haskell 中，List 是一种单一类型的数据结构，可以用来存储多个类型相同的元素。

### 定义列表

```shell
ghci> let lostNumbers = [4, 8, 15, 16, 23, 48]
ghci> lostNumbers
[4,8,15,16,23,48]
```

> GHCi 中定义常量可以使用`let`关键字，在GHCi 中执行`let a = 1`与在脚本中编写`a = 1`是等价的。

### 合并列表

两个 List 可以通过 `++`运算符来进行合并：

```shell
ghci> [1, 2, 3] ++ [4, 5, 6]
[1,2,3,4,5,6]
```

> 合并列表的原理是遍历左侧的列表，从后往前依次插入到右侧的列表中。

### 插入元素

通过`:`运算符可以将元素插入列表：

```shell
ghci> 1:[2]
[1,2]
ghci> 1:2:[3]
[1,2,3]
```

### 获取元素

通过`!!`运算符可以根据索引获取对应的列表元素：

```shell
ghci> [1, 2, 3] !! 1
2
```

> Haskell 中的列表不允许越界读取，否则会抛出异常。

### 列表嵌套

List 中的元素可以是一个 List。List 中的 List 可以是不同长度，但是类型必须相同。

```shell
ghci> let b = [[1, 2, 3], [5, 3, 3, 3], [1, 2]]
```

### 列表比较

当 List 中装有可比较的元素时，可以使用比较运算符进行比较。列表会先比较第一个元素，若结果相等，则比较下一个，以此类推。

```shell
ghci> [3, 4] > [1, 2]
True
ghci> [1, 2] > [0, 2]
True
ghci> [3, 4, 2] > [3, 4]
True
ghci> [1, 2] == [1, 2]
True
```

### 常用函数

**head**：获取首个元素

```shell
ghci> head [1, 2, 3]
1
```

**tail**：获取除首个元素外剩余元素

```shell
ghci> tail [1, 2, 3]
[2,3]
```

**last**：获取最后一个元素

```shell
ghci> last [1, 2, 3]
3
```

**init**：获取除最后一个元素外剩余元素

```shell
ghci> init [1, 2, 3]
[1,2]
```

**length**：获取列表长度

```shell
ghci> length [1, 2, 3]
3
```

**null**：列表是否为空

```shell
ghci> null []
True
```

**reverse**：反转列表

```shell
ghci> reverse [1, 2, 3]
[3,2,1]
```

**take**：从头部开始截取 n 个元素

```shell
ghci> take 2 [1, 2, 3]
[1,2]
```

**drop**：从头部开始剔除 n 个元素

```shell
ghci> drop 1 [1,2,3]
[2,3]
```

**maximum**：求最大值

```shell
ghci> maximum [1, 2, 3]
3
```

**minimum**：求最小值

```shell
ghci> minimum [1, 2, 3]
1
```

**sum**：求和

```shell
ghci> sum [1, 2, 3]
6
```

**product**：求乘积

```shell
ghci> product [1, 2, 3]
6
```

**elem**：判定元素是否存在（通常以中缀函数的形式调用）

```shell
ghci> 3 `elem` [1, 2, 3]
True
```

**replicate**：对一个参数复制 n 次，并返回一个列表

```shell
ghci> replicate 3 6
[6,6,6]
```
