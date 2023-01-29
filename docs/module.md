# 模块

Haskell 中的模块是含有一组相关的函数，类型和类型类的组合。

### 加载模块

加载模块的语法为`import <module name>`，这必须得在函数的定义之前，一般都是至于代码的顶部。多个模块只需要将`import`语句分行写开即可。

```haskell
import Data.List
import Data.Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
```

在 GHCi 中加载模块需要使用`:m`命令，多个模块使用空格隔开。

```shell
ghci> :m Data.List Data.Map Data.Set
```

#### 按需加载

如果只用到模块的部分函数，可以仅加载它们。例如，仅加载`Data.List`模块的`nub`和`sort`函数。

```haskell
import Data.List (nub, sort)
```

也可以只包含除指定函数之外的其他函数，这在避免多个模块中函数的命名冲突时很有用。例如，加载`Data.List`模块除`nub`函数外的其他函数。

```haskell
import Data.List hiding (nub)
```

#### 避免命名冲突

模块之间难免会出现同名的函数，同时引入就会出现命名冲突的问题。如`Data.List`模块中的`filter`函数，就与默认加载的`Prelude`模块中的`filter`函数冲突。

使用`qualified import`可以有效的避免命名冲突。

```haskell
import qualified Data.List
```

`qualified import`需要使用按键索值的方式才能调用函数。如`Data.List`中的`filter`函数，必须得`Data.List.filter`。

如果觉得名称过长，可以用`as`起个别名。

```haskell
import qualified Data.List as L
```

再调用`filter`函数仅需`L.filter`即可。

### 定义模块

模块的开头定义模块的名称，如果文件名叫`Geometry.hs`那它的名字就得是`Geometry`。在声明出它含有的函数名之后就可以编写函数的实现了。

Geometry.hs

```haskell
module Geometry (
  sphereVolume,
  sphereArea,
  cubeVolume,
  cubeArea,
  cuboidVolume,
  cuboidArea
) where


sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
```

`Geometry`模块中的`rectangleArea`函数未被声明导出，所以仅能在模块内使用。

#### 子模块

模块也可以按照分层的结构来组织，每个模块都可以有多个子模块，子模块还可以有自己的子模块。例如`Geometry`模块可以被分成三个子模块。

首先创建一个`Geometry`文件夹，在里面创建三个文件：

Sphere.hs

```haskell
module Geometry.Sphere (
  volume,
  area,
) where

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)
```

Cube.hs

```haskell
module Geometry.Cube (
  volume,
  area,
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side
```

Cuboid.hs

```haskell
module Geometry.Cuboid (
  volume,
  area,
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
```

计算球体面积可以加载`Geometry.Sphere`模块，调用`area`函数。

```haskell
ghci> :m Geometry.Sphere
ghci> area 12.4
1932.2051
```

若要用到两个或更多的模块，就必须得`qualified import`来避免重名。

```haskell
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube
```
