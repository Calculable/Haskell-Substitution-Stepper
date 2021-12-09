module Source5 where
import Data.Maybe(isNothing)

x = [1, 2, 3]

y = first (map (+1) [1, 2, 3, 4, 5])

{-infinite lists-}

a = reverse ([] :: [Int]) 
b = isNothing (Just 5)

-- infiniteListInput = sumOfTheFirstXElements [1..] 3

first :: [a] -> a
first (x:xs) = x

sumOfTheFirstXElements :: (Num a) => [a] -> Integer -> a
sumOfTheFirstXElements _ 0 = 0
sumOfTheFirstXElements [] _ = 0
sumOfTheFirstXElements (x:xs) amountOfElements = x + (sumOfTheFirstXElements xs (amountOfElements - 1))


-- override'enumFrom :: Enum a => a -> [a]
-- override'enumFrom x = x: (overwrite'enumFrom (succ x))

--(this is a good example for recursive let bindings)
--z = [(i,j) | i <- [1,2],
--         j <- [1..4] ]

override'map :: (a -> b) -> [a] -> [b]
override'map f []     = []
override'map f (x:xs) = f x : override'map f xs