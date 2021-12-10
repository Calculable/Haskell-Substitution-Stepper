module IntegrationTestBindings where
import Data.Maybe
import Data.Either

{-Arithmetic-}
additionInput = 1+1
additionExpectedOutput = 2

additionWithApplicationSyntaxInput = (+) 1 1
additionWithApplicationSyntaxExpectedOutput = 2

substractionInput = 1-1
substractionExpectedOutput = 0

nestedArithmeticInput = (1 + 10) - (2 * 3)
nestedArithmeticExpectedOutput = 5


doubleAdditionInput = 1.5 + 1.5 
doubleAdditionExpectedOutput = 3

{-Function Application-}


functionApplicationInput = add 1 2
functionApplicationExpectedOutput = 3


lamdaApplicationInput = (\x y -> x+y) 1 2
lamdaApplicationExpectedOutput = 3


nestedApplicationInput = add (add 0 1) (add 0 2)
nestedApplicationExpectedOutput = 3

{-Higher Order Functions-}


higherOrderResultInput = (multiplicator 10) 5
higherOrderResultExpectedOutput = 50


higherOrderParameterInput = twice double 1
higherOrderParameterExpectedOutput = 4

{-basic operations on Boolean Type-}

basicOperationOnBooleanInput = True == False
basicOperationOnBooleanExpectedOutput = False

{-basic operations on Char Type -}

basicOperationOnCharInput = succ 'a'
basicOperationOnCharExpectedOutput = 'b' :: Char

{-basic operations on Double Type-}


basicOperationOnDoubleInput = 1.3 - 0.3
basicOperationOnDoubleExpectedOutput = 1.0


{-basic operations on Int Type-}

basicOperationOnIntInput = (2 :: Int) - (1 :: Int)
basicOperationOnIntExpectedOutput = 1 :: Int

{-basic operations on Integer Type-}


basicOperationOnIntegerInput = (2 :: Integer) - (1 :: Integer)
basicOperationOnIntegerExpectedOutput = 1 :: Integer

{-basic operations on Either Type-}



basicOperationOnEitherInput = fromRight (Right 4)
basicOperationOnEitherExpectedOutput = 4

{-basic operations on Maybe Type-}


basicOperationOnMaybeInput = isNothing (Just 5)
basicOperationOnMaybeExpectedOutput = False

{-basic operations on Rational Type-}


basicOperationOnRationalInput = (2 :: Rational) > (1 :: Rational) 
basicOperationOnRationalExpectedOutput = True

{-basic operations on String Type-}

basicOperationOnStringInput = "Hallo" < "World"
basicOperationOnStringExpectedOutput = True

{-basic operations on List Type-}

basicOperationOnListInput = [1, 2, 3] < [4, 5, 6]
basicOperationOnListExpectedOutput = True

{-basic operations on Tuple Type-}

basicOperationOnTupleInput = (1, 2) <  (3, 4)
basicOperationOnTupleExpectedOutput = True

{-Support for Num Type Instance-}

absInput = abs (-3)
absExpectedOutput = 3

signumInput = signum 5 
signumExpectedOutput = 1

signumWithDoubleInput = signum 5.3 
signumWithDoubleExpectedOutput = 1

{-Support for Fractional Type Instance-}

divisionInput = 1.0 / 2.0
divisionExpectedOutput = 0.5

recipInput = recip 2.0
recipExpectedOutput = 0.5

{-Support for EQ Type Instance-}

equalsCharInput = (==) 'a' 'a'
equalsCharExpectedOutput = True

equalsIntegerInput = (==) 1 1
equalsIntegerExpectedOutput = True


equalsDoubleInput = (==) 1.0 1.0
equalsDoubleExpectedOutput = True


equalsStringInput = (==) "Hello" "Hello"
equalsStringExpectedOutput = True

notEqualsInput = (/=) "Hello" "World"
notEqualsExpectedOutput = True


equalsForNotEqualStringInput = (==) "Hello" "World"
equalsForNotEqualStringExpectedOutput = False

{-Support for Ord Type Instance-}


lessOrEqualInput = (<=) 'a' 'a'
lessOrEqualExpectedOutput = True

lessInput = (<) 1 1
lessExpectedOutput = False

greaterOrEqualInput = (>=) "Hello" "Hello"
greaterOrEqualExpectedOutput = True

maxInput = max "Hello" "World"
maxExpectedOutput = "World"

{-Support for Enum Type Instance-}

succWithIntegerInput = succ 1
succWithIntegerExpectedOutput = 2

succWithDoubleInput = succ 2.0
succWithDoubleExpectedOutput = 3.0

{-Support for Floating Type Instance-}

expInput = exp 0
expExpectedOutput = 1

logInput = log 1
logExpectedOutput = 0

sqrtInput = sqrt 4
sqrtExpectedOutput = 2

powerInput =  (abs (((**) 2.0 3.0) - 8.0)) < 0.0001
powerExpectedOutput = True

cosinusInput = cos 0
cosinusExpectedOutput = 1

{-Support for Integral Type Instance-}


integerDivisionInput = 4 `div` 2
integerDivisionExpectedOutput = 2


moduloInput = 10 `mod` 2
moduloExpectedOutput = 0

{-Support for RealFrac Type Instance-}

floorInput = floor 5.9
floorExpectedOutput = 5

{-Polymorphism-}


polymorphicFunctionInput = polymorphicFunction 1 "Test"
polymorphicFunctionExpectedOutput = 42


polymorphicFunctionWithTypeConstraintInput = polymorphicFunctionWithTypeConstraint 1 "Test"
polymorphicFunctionWithTypeConstraintExpectedOutput = 42


functionWithPolymorphicResultInput = polymorphicResult (1 :: Integer)
functionWithPolymorphicResultExpectedOutput = 42

{-NonStrictness-}


nonStrictnessInput = ignoreParameter (1.0 / 0.0)
nonStrictnessExpectedOutput = 42

{-Pattern Matching-}


integerPatternMatchingInput = integerPatternMatching 2 
integerPatternMatchingExpectedOutput = "Two!"


stringPatternMatchingInput = stringPatternMatching "42"
stringPatternMatchingExpectedOutput = True


booleanPatternMatchingInput = booleanPatternMatching False
booleanPatternMatchingExpectedOutput = True


multiArgumentPatternMatchingInput = multiArgumentPatternMatching True True
multiArgumentPatternMatchingExpectedOutput = True


patternMatchingOnEmptyListInput = patternMatchingOnIntegerList []
patternMatchingOnEmptyListExpectedOutput = 42


patternMatchingOnIntegerListInput = patternMatchingOnIntegerList [1, 2, 3, 4, 5]
patternMatchingOnIntegerListExpectedOutput = 1


patternMatchingOnPolymorphicListInput = patternMatchingOnPolymorphicList ["Hallo", "Welt"]
patternMatchingOnPolymorphicListExpectedOutput = "Hallo"


patternMatchingOnAnyConstructorInput = patternMatchingOnAnyConstructor (Just 5)
patternMatchingOnAnyConstructorExpectedOutput = 5


patternMatchingOnUnsupportedTypeInput = customIsNothing (Just 5)
patternMatchingOnUnsupportedTypeExpectedOutput = False

{-Recursion-}


recursionInput = findMaximum [1, 2, 3, 4, 42, 5]
recursionExpectedOutput = 42

{-List handling-}


listOperationsInput = reverseList [1, 2, 3]
listOperationsExpectedOutput = [3, 2, 1]

{-where-}
whereInput = functionWithWhere 1
whereExpectedOutput = 16

multipleWhereInput = functionWithMultipleWhere 1
multipleWhereExpectedOutput = 1


{-do notation / let bindings-}
functionWithDoAndLetInput = functionWithDoAndLet 1
functionWithDoAndLetExpectedOutput = 8

{-Tuples-}
tupleAsParameterInput = second (1, 2)
tupleAsParameterExpectedOutput = 2

equalsOnTupleInput = (1, 2) == (1, 3)
equalsOnTupleExpectedOutput = False


{-custom types-}

functionOnCustomTypeInput = getData (Top 5)
functionOnCustomTypeExpectedOutput = 5

equalityOnCustomTypeInput = changeDirection (Top 5)
equalityOnCustomTypeExpectedOutput = Down 5

{-infinite lists-}
infiniteListInput = sumOfTheFirstXElements [1..] 3
infiniteListExpectedOutput = 6

{-map-}
mapInput = first (map (+1) [1, 2, 3, 4, 5])
mapExpectedOutput = 2

{-fmap on maybe-}
fmapOnJustInput = getMaybeValue (fmap (+1) (Just (5 :: Integer)))
fmapOnJustExpectedOutput = 6

fmapOnNothingInput = isNothing (fmap (+1) Nothing)
fmapOnNothingExpectedOutput = True

{-generator-}

generatorInput = length [(i,j) |    i <- [1,2],
                                    j <- [1..4] ]

generatorExpectedOutput :: Integer
generatorExpectedOutput = 8

{-Monad maybe-}

monadMaybeInput = (getMaybeValue (monadicFunction (Just 4)))
monadMaybeExpectedOutput = "Hallo"


{-Helper Functions-}

add :: Int -> Int -> Int 
add x y = x + y

addForInteger :: Integer -> Integer -> Integer 
addForInteger x y = x + y

double :: Int -> Int 
double x = 2*x

twice :: (Int -> Int) -> Int -> Int 
twice function number = function (function number)

multiplicator :: Int -> (Int -> Int)
multiplicator x = (\y -> x*y)

ignoreParameter :: a -> Int
ignoreParameter _ = 42

polymorphicFunction :: a -> b -> Int
polymorphicFunction _ _ = 42

polymorphicFunctionWithTypeConstraint :: (Num a, Ord b) => a -> b -> Int
polymorphicFunctionWithTypeConstraint _ _ = 42

polymorphicResult :: (Num a, Num b) => a -> b
polymorphicResult a = fromInteger 42


integerPatternMatching :: Integer -> String
integerPatternMatching 1 = "One!"
integerPatternMatching 2 = "Two!"
integerPatternMatching x = "Not between 1 and 3"

stringPatternMatching :: String -> Bool
stringPatternMatching "42" = True
stringPatternMatching _ = False

booleanPatternMatching :: Bool -> Bool
booleanPatternMatching True = False
booleanPatternMatching False = True

multiArgumentPatternMatching :: Bool -> Bool -> Bool
multiArgumentPatternMatching True True = True
multiArgumentPatternMatching _ _ = False

patternMatchingOnIntegerList :: [Integer] -> Integer
patternMatchingOnIntegerList [] = 42
patternMatchingOnIntegerList (a:bc) = a

patternMatchingOnPolymorphicList :: [a] -> a
patternMatchingOnPolymorphicList (a:bc) = a

patternMatchingOnAnyConstructor :: Maybe Int -> Int
patternMatchingOnAnyConstructor (Just x) = x
patternMatchingOnAnyConstructor Nothing = 42

customIsNothing :: Maybe a -> Bool
customIsNothing (Just _) = False
customIsNothing Nothing = True

findMaximum :: (Ord a) => [a] -> a  
findMaximum [x] = x  
findMaximum (x:xs) = max x (findMaximum xs)  

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (a:bc) = reverseList bc ++ [a]

functionWithWhere :: Integer -> Integer
functionWithWhere n = x * x
    where x = (n + 1) * 2


functionWithDoAndLet :: Integer -> Integer
functionWithDoAndLet x = do
    let y = x+x
    let z = y+y
    z + z    

functionWithMultipleWhere :: Integer -> Integer
functionWithMultipleWhere x = (y * z)
    where
        y = x
        z = 1


data Direction a = Top a | Down a deriving (Eq, Ord, Show, Read)

changeDirection :: Direction a -> Direction a
changeDirection (Top a) = Down a
changeDirection (Down a) = Top a

getData :: Direction a -> a
getData (Top a) = a
getData (Down a) = a

second :: (a, b) -> b
second (x, y) = y

override'enumFrom :: Enum a => a -> [a]
override'enumFrom x = x: (override'enumFrom (succ x))

override'enumFromTo :: Enum a => a -> [a]
override'enumFromTo x = x: (override'enumFrom (succ x))

override'enumTo:: (Ord a, Enum a) => a -> a -> [a]
override'enumTo x y = if (x == y)
                        then [x]
                        else x: (override'enumTo (succ x) y)

sumOfTheFirstXElements :: (Num a) => [a] -> Integer -> a
sumOfTheFirstXElements _ 0 = 0
sumOfTheFirstXElements [] _ = 0
sumOfTheFirstXElements (x:xs) amountOfElements = x + (sumOfTheFirstXElements xs (amountOfElements - 1))


override'map :: (a -> b) -> [a] -> [b]
override'map f []     = []
override'map f (x:xs) = f x : map f xs

first :: [a] -> a
first (x:xs) = x

monadicFunction :: Maybe Int -> Maybe String
monadicFunction maybeValue = do
    value <- maybeValue
    return "Hallo"

getMaybeValue :: Maybe a -> a
getMaybeValue (Just x) = x

override'isNothing :: Maybe a -> Bool
override'isNothing Nothing = True
override'isNothing _ = False

override'length           :: [a] -> Int
override'length []        =  0
override'length (_:l)     =  1 + length l