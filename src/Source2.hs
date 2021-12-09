module Source2 where

sayMe :: Integer -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Not between 1 and 3"

aPatternMatchingOnValueDemo = sayMe 4

a = True

x = 1
y = 2
z = x


r = 1 + (2 * 3)

namedValueDemo = 1 - y

arithmeticDemo = 1 + 2 * 3


lamdaDemo = (\x -> x + x)

lamdaApplicationDemo = lamdaDemo 1

usageOfPreludeFunctionsDemo = abs (negate 3)

isTrue :: Bool -> String
isTrue True = "Yes, its True" 
isTrue False = "No its false" 

q = isTrue True

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (a:bc) = reverseList bc ++ [a]

patternMatchingOnOperatorDemo :: [Integer]
patternMatchingOnOperatorDemo = reverseList [1, 2, 3]

test :: Integer -> String
test _ = "some string"

applyTest = test 4

f :: Bool -> Bool -> Bool
f x y = not x && y

double :: Num a => a -> a
double x = x + x

doubleTest = double 1

doubleValueTest = 0.5
cosinusTest = cos 0.5

functionWithWhere :: Integer -> Integer
functionWithWhere n = x * x
    where x = (n + 1) * 2

letBindingTest = functionWithWhere 1


multiArgumentPatternMatching :: Bool -> Bool -> Bool
multiArgumentPatternMatching True True = True
multiArgumentPatternMatching _ _ = False

multiArgumentPatternMatchingTest = multiArgumentPatternMatching True True

infiniteListInput = sumOfTheFirstXElements [1..] 3

sumOfTheFirstXElements :: (Num a) => [a] -> Integer -> a
sumOfTheFirstXElements _ 0 = 0
sumOfTheFirstXElements [] _ = 0
sumOfTheFirstXElements (x:xs) amountOfElements = x + (sumOfTheFirstXElements xs (amountOfElements - 1))


overwrite'enumFrom :: Enum a => a -> [a]
overwrite'enumFrom x = x: (overwrite'enumFrom (succ x))