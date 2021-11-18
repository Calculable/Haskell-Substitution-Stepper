module Source2 where


x = 1 + 2 * 3

sayMe :: Integer -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

y = 3 - 4
z = x + y

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (a:bc) = reverseList bc ++ [a]

abc = reverseList [1, 2, 3]

a = sayMe 1

b = z

c = (\x -> x + x)

d = c 1