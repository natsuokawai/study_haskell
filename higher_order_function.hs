-- curried function
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z
-- example
-- ghci> let multTwoWithNine = multThree 9  
-- ghci> multTwoWithNine 2 3  
-- 54  


-- applyTwice
-- takes a function and then applies it twice
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


-- map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs
-- example
-- let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
-- [[1,2,3],[3,4,5],[2,2]]

-- quicksort'
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallerSorted = quicksort' (filter (<=x) xs)
      biggerSorted  = quicksort' (filter (>x) xs)
  in  smallerSorted ++ [x] ++ biggerSorted

-- largestDivisible
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

-- chain
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n  = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

-- lambdas
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- zipWith (/a b -> (a * 30 + 3) / b) [5,4,3,2,1][1,2,3,4,5]
-- map (\( a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

sum_by_foldl :: (Num a) => [a] -> a
sum_by_foldl xs = foldl (\acc x -> acc + x) 0 xs




