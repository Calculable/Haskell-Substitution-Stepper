module Source2 where

x = 1
y = 2
namedValueDemo = 1 - y

arithmeticDemo = 1 + 2 * 3


lamdaDemo = (\x -> x + x)

lamdaApplicationDemo = lamdaDemo 1

usageOfPreludeFunctionsDemo = abs (negate 3)

sayMe :: Integer -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Not between 1 and 3"

patternMatchingOnValueDemo = sayMe 1

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (a:bc) = reverseList bc ++ [a]

patternMatchingOnOperatorDemo = reverseList [1, 2, 3]