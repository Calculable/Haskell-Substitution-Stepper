module StepperProofOfConcept
  ( printExampleStepping,
  )
where

import Data.List
import Data.Bool
{-import System.Eval.Haskell-}

type BindingReference = String {-In an Expression x = 1, "x" would become the BindingReference-}
type Operator = String
type Binding = (BindingReference, Expression)

data Expression
    = BindingReference String
    | Value Int
    | OperatorApplication { leftExpression :: Maybe Expression
                     , operator :: Operator
                     , rightExpression :: Maybe Expression
                     }


instance Show Expression where
  show (BindingReference reference) = reference
  show (Value value) = show value
  show (OperatorApplication (Just leftExpression) operator (Just rightExpression)) = "(" ++ show leftExpression ++ " " ++ operator ++ " " ++ show rightExpression ++ ")"
  show (OperatorApplication leftExpression operator rightExpression) = "(" ++ show leftExpression ++ " " ++ operator ++ " " ++ show rightExpression ++ ")"

solve :: [(BindingReference, Expression)] -> Maybe Expression -> Maybe Int
solve _ Nothing = Nothing
solve _ (Just (Value value)) = Just value
solve bindings (Just (BindingReference reference)) = solve bindings (findBinding reference bindings)
solve bindings (Just (OperatorApplication leftExpression operator rightExpression)) = do
    leftValue <- solve bindings leftExpression
    rightValue <- solve bindings rightExpression
    operatorFunction <- getOperatorFunction operator
    return (operatorFunction leftValue rightValue)

{-solution with eval -}
{-solve bindings (Just (OperatorApplication leftExpression operator rightExpression)) = do
    leftValue <- solve bindings leftExpression
    rightValue <- solve bindings rightExpression
    operatorResult <- eval "1 + 1" [] :: IO (Maybe Int)
    return operatorResult-}

applyStep :: [(BindingReference, Expression)] -> Maybe Expression -> Maybe Expression
applyStep _ Nothing = Nothing
applyStep _ (Just (Value value)) = Just (Value value) {-cannot reduce more-}
applyStep bindings (Just (BindingReference reference)) = findBinding reference bindings {-replace binding reference with actual expression (Delta Reduction)-}
applyStep bindings (Just (OperatorApplication (Just (Value leftValue)) operator (Just (Value rightValue)))) = do
    operatorFunction <- getOperatorFunction operator
    return (Value (operatorFunction leftValue rightValue))
applyStep bindings (Just (OperatorApplication (Just (Value leftValue)) operator rightExpression)) = Just (OperatorApplication (Just (Value leftValue)) operator (applyStep bindings rightExpression))
applyStep bindings (Just (OperatorApplication leftExpression operator rightExpression)) = Just (OperatorApplication (applyStep bindings leftExpression) operator rightExpression)

getOperatorFunction :: Num a => String -> Maybe (a -> a -> a)
getOperatorFunction "+" = Just (+)
getOperatorFunction "-" = Just (-)
getOperatorFunction "*" = Just (*)
{-no div yet because the Integer type has no Fractional instance.-}
getOperatorFunction x = Nothing

findBinding :: String -> [Binding] -> Maybe Expression
findBinding key bindings = do foundBinding <- find (\ (x, _) -> x == key) bindings
                              return (snd foundBinding)

printStepByStepReduction :: [(BindingReference, Expression)] -> Maybe Expression -> IO ()
printStepByStepReduction _ Nothing = print "Nothing"
printStepByStepReduction _ (Just (Value x)) = print x
printStepByStepReduction bindings (Just expression) = do
    print expression
    printStepByStepReduction bindings (applyStep bindings (Just expression))

printBindings :: [Binding] -> IO ()
printBindings [] = return ()
printBindings ((reference, expression):xs) = do
    putStrLn (reference ++ " = " ++ show expression)
    printBindings xs

exampleBindings = [
     ("x", OperatorApplication (Just (Value 1)) "+" (Just (OperatorApplication (Just (Value 2)) "*" (Just (Value 3))))), {-Represents the expression "x = 1+2*3"-}
     ("y", OperatorApplication (Just (Value 3)) "-" (Just (Value 4))), {-Represents the expression "y = 3-4"-}
     ("z", OperatorApplication (Just (BindingReference "x")) "+" (Just (BindingReference "y"))) {-Represents the expression "x + y"-}
     ]
exampleExpression = BindingReference "z"

printExampleStepping :: IO ()
printExampleStepping = do
    putStrLn "\nBindings"
    printBindings exampleBindings

    putStrLn "\nExpression to Step"
    print exampleExpression

    putStrLn "\nReduction"
    printStepByStepReduction exampleBindings (Just exampleExpression)