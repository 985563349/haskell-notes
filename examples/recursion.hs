-- factorial
factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- maximum'
maximum' :: Integral a => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- minimum'
minimum' :: Integral a => [a] -> a
minimum' [] = error "minimum of empty list"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

-- fibonacci
fibinacci :: Integral a => a -> a
fibinacci 0 = 0
fibinacci 1 = 1
fibinacci n = fibinacci (n - 1) + fibinacci (n - 2)

-- replicate'
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n - 1) x

-- length'
length' :: Num b => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- zip'
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- elem'
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs

-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted
