# 递归

递归是指函数定义中使用函数自身的方法。

递归在 Haskell 中至关重要，由于没有`while`和`for`循环，递归就成为了替代方案。

### 递归实作

**maximum**：

```haskell
maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum of empty list."
maximum' [x] = x
maximum' (x:xs)
         | x > maxTail = x
         | otherwise = maxTail
         where maxTail = maximum' xs
```

递归和模式匹配能灵活的组合使用，大多数命令式语言里没有模式匹配，只能靠一堆`if else`来测试边界。

使用`max`函数简化`maximum`函数实现：

```haskell
maximum' :: Ord => [a] -> a
maximum' [] = error "maximum of empty list."
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
```

一个`List`的最大值就是它的首个元素与尾部元素中最大值比较的结果，简明扼要。

![maximum](./accets/maximum.png)

**replicate**：

```haskell
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
           | n <= 0 = []
           | otherwise = x:replicate' (n - 1) x
```

`replicate`函数中使用了守卫而非模式匹配，是因为需要做布尔判断，如果`n <= 0`就返回一个空 List，否则返回以`x`做首并后接重复`n - 1`次`x`的 List，最后`n - 1`部分会令函数抵达边缘条件。

> `Num`不是`Ord`的子集，表示数字不一定得拘泥于排序，在做加减法比较时要将`Num`与`Ord`类型约束区别开。

**take**：

```haskell
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
      | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n - 1) xs
```

**quicksort**：

```haskell
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted
```

快速排序思想：使所有小于等于头部的元素在先（已完成排序），后跟大于头部的元素（同样已完成排序）。

![quicksort](./accets/quicksort.png)

### 递归的思考

使用递归解决问题时应当先考虑递归会在什么条件下不可用，然后再找出它的边界条件和单位元，考虑参数应该在何时切开（如对 List 使用模式匹配），以及在何处执行递归。
