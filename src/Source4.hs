module Source4 where

findMaximum :: (Ord a) => [a] -> a  
--findMaximum [] = error "maximum of empty list"  
findMaximum [x] = x  
findMaximum (x:xs) = max x (findMaximum xs)  

example = findMaximum [1, 2, 3]
