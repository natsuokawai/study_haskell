-- data: the keyword to define a type
-- example
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2- x1) * (abs $ y2 - y1)

-- surface $ Rectangle 0 0 2 2
-- -> 4.0
-- surface $ Circle 0 0 3
-- -> 28.274334

-- modify above example
data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)
surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2- x1) * (abs $ y2 - y1)
