module Source5 where
import Data.Maybe(isNothing)

x = [1, 2, 3]

{-infinite lists-}

a = reverse ([] :: [Int]) 
b = isNothing (Just 5)

-- infiniteListInput = sumOfTheFirstXElements [1..] 3

y = first (map (+1) [1, 2, 3, 4, 5])

z = fmap (+1) (Just (5 :: Integer))
r = fmap (+1) Nothing

first :: [a] -> a
first (x:xs) = x

override'map :: (a -> b) -> [a] -> [b]
override'map f []     = []
override'map f (x:xs) = f x : override'map f xs

sumOfTheFirstXElements :: (Num a) => [a] -> Integer -> a
sumOfTheFirstXElements _ 0 = 0
sumOfTheFirstXElements [] _ = 0
sumOfTheFirstXElements (x:xs) amountOfElements = x + (sumOfTheFirstXElements xs (amountOfElements - 1))


-- override'enumFrom :: Enum a => a -> [a]
-- override'enumFrom x = x: (overwrite'enumFrom (succ x))

--(this is a good example for recursive let bindings)
--z = [(i,j) | i <- [1,2],
--         j <- [1..4] ]

monadTest = monadicFunction (Just 5)

monadicFunction :: Maybe Int -> Maybe String
monadicFunction maybeValue = do
    a <- maybeValue
    return "Hallo"

generatorInput = [(i,j) |   i <- [1,2],
                            j <- [1..4] ]


override'length           :: [a] -> Int
override'length []        =  0
override'length (_:l)     =  1 + length l