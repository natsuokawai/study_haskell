-- notes
-- non-exhaustive patterns: patterns are not enough
-- pattern match is redundant: too many patterns

-- factorial
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- addVectors
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- imprementation of `head` function
head' :: [a] -> a
head' [] = error "Cant't call head of an empty list, dummy!"
head' (x:_) = x

-- imprementation of `length` function
length' :: (Integral b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

-- imprementation of `sum` function
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- BMI function
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | bmi <= 30.9 = "You're fat! Lose some weight, fatty!"  
  | otherwise   = "You're a whale, congratulations!"  

bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise                   = "You're a whale, congratulations!"

bmiTell3 :: (RealFloat a) => a -> a -> String
bmiTell3 weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"  
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2

bmiTell4 :: (RealFloat a) => a -> a -> String
bmiTell4 weight height
  | bmi <= skinny = "You're underweight, you emo, you!"  
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise   = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 35.0

-- function taht return initial of the input name
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

-- funciton that takes list of weight-height pairs and returns a list of BMIs
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2


-- function that calculates the surface area of cylinder
-- using let ~ in ~ syntax
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea  = pi * r ^ 2
  in  sideArea + 2 * topArea

-- > let bindings are expressions and are fairly local in their scope, they can't be used across guards


-- imprementation of `head` function using `case` syntax
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x
