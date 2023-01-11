# 字符串

haskell 中的字符串实际上就是一组字符的 List，如`"Hello"`只是`['h', 'e', 'l', 'l', 'o']`的语法糖。所以处理 List 的函数同样可以用来处理字符串。

### 定义字符串

```shell
ghci> let text = "hello"
```

### 合并字符串

```shell
ghci> "he" ++ "llo"
"hello"
```

### 插入字符

```shell
ghci> 'h':"ello"
"hello"
```

### 获取字符

```shell
ghci> "hello" !! 1
'e'
```

### 字符串比较

```shell
ghci> "ab" > "cd"
False
ghci> "ab" == "ab"
True
```

### 常用函数

```shell
ghci> head "hello"
'h'
ghci> tail "hello"
"ello"
ghci reverse "hello"
"olleh"
```
