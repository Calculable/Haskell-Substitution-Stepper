module Source5 where

x = [1, 2, 3]

y = take 1 [1, 2, 3, 4, 5]

{-infinite lists-}

infiniteListInput = sumOfTheFirstXElements [1..] 3

sumOfTheFirstXElements :: (Num a) => [a] -> Integer -> a
sumOfTheFirstXElements _ 0 = 0
sumOfTheFirstXElements [] _ = 0
sumOfTheFirstXElements (x:xs) amountOfElements = x + (sumOfTheFirstXElements xs (amountOfElements - 1))


overwrite'enumFrom :: Enum a => a -> [a]
overwrite'enumFrom x = x: (overwrite'enumFrom (succ x))

--(this is a good example for recursive let bindings)
--z = [(i,j) | i <- [1,2],
--         j <- [1..4] ]