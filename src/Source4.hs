module Source4 where

findMaximum :: (Ord a) => [a] -> a  
--findMaximum [] = error "maximum of empty list"  
findMaximum [x] = x  
findMaximum (x:xs) = max x (findMaximum xs)  

example = findMaximum [1, 2, 3]

sumOfTheFirstThreeElements :: (Num a) => [a] -> a
sumOfTheFirstThreeElements [x, y, z, _] = (x+y)+z

example2 = sumOfTheFirstThreeElements [1..]
