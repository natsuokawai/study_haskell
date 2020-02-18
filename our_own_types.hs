import qualified Data.Map as Map
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

-- nudges
nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point x y) r) a b = Circle' (Point (x+a) (y+b)) r
nudge (Rectangle' (Point x1 y1) (Point x2 y2)) a b = Rectangle' (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- record type
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- type synonyms
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                            then Right code
                            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- recursive data structures
data List a = Empty | Cons { listHead :: a, listTail :: List a } deriving (Show, Read, Eq, Ord)

-- binary search tree
