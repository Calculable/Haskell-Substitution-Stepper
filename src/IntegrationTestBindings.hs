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

