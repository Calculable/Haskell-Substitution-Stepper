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

{-Support for RealFloat Type Instance-}

isNaNInput = isNaN (1.0 / 0.0)
isNaNExpectedOutput = True

{-error handling-}
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

