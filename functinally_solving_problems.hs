-- Reverse Polish notation calculator

import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (x * y):ys
        foldingFunction (x:y:ys) "+" = (x + y):ys
        foldingFunction (x:y:ys) "-" = (y - x):ys
        foldingFunction (x:y:ys) "/" = (y / x):ys
        foldingFunction (x:y:ys) "^" = (y ** x):ys
        foldingFunction (x:xs) "ln"  = log x:xs
        foldingFunction xs "sum"     = [sum xs]
        foldingFunction xs numberString = read numberString:xs


-- Heathrow to London
data Node = Node Road (Maybe Road)
data Road = Road Int Node
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA = sum $ map snd pathA
      priceB = sum $ map snd pathB
      forwardPriceToA = priceA + a
      crossPriceToA = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      newPathToA = if forwardPriceToA <= crossPriceToA
                   then (A,a):pathA
                   else (C,c):(B,b):pathB
      newPathToB = if forwardPriceToB <= crossPriceToB
                   then (B,b):pathB
                   else (C,c):(A,a):pathA
  in (newPathToA, newPathToB)
