module TypedStepperProofOfConcept
  (printBindings, printStepByStepReduction, Expression(..)
  )
where

import Data.List
import Data.Bool

{-Types-}
type Operator = String
type ReductionStepDescription = String --for example: "replace x with definition"
type Binding = (String, Expression)
type FunctionArgument a = ((Expression -> a), Expression)

data Expression
    = StringValue String
    | IntegerValue Integer
    | DoubleValue Double
    | BoolValue Bool
    | CharValue Char
    | ListValue [Expression]
    | TupleValue (Expression, Expression)
    | OperatorApplication { leftExpression :: Expression
                     , operator :: Operator
                     , rightExpression :: Expression
                     }
    | BindingReference String
    | InvalidExpression String

{-Type Classes-}
instance Num Expression where
    (+) (IntegerValue x) (IntegerValue y) = IntegerValue ((Prelude.+) x y)
    (+) (DoubleValue x) (DoubleValue y) = DoubleValue ((Prelude.+) x y)
    (+) (IntegerValue x) (DoubleValue y) = DoubleValue ((Prelude.+) (fromInteger x) y)
    (+) (DoubleValue x) (IntegerValue y) = DoubleValue ((Prelude.+) x (fromInteger y))
    (+) _ _ = InvalidExpression "+ not supported by this type"
    (-) (IntegerValue x) (IntegerValue y) = IntegerValue ((Prelude.-) x y)
    (-) (DoubleValue x) (DoubleValue y) = DoubleValue ((Prelude.-) x y)
    (-) (IntegerValue x) (DoubleValue y) = DoubleValue ((Prelude.-) (fromInteger x) y)
    (-) (DoubleValue x) (IntegerValue y) = DoubleValue ((Prelude.-) x (fromInteger y))
    (-) _ _ = InvalidExpression "- not supported by this type"
    (*) (IntegerValue x) (IntegerValue y) = IntegerValue ((Prelude.*) x y)
    (*) (DoubleValue x) (DoubleValue y) = DoubleValue ((Prelude.*) x y)
    (*) (IntegerValue x) (DoubleValue y) = DoubleValue ((Prelude.*) (fromInteger x) y)
    (*) (DoubleValue x) (IntegerValue y) = DoubleValue ((Prelude.*) x (fromInteger y))
    (*) _ _ = InvalidExpression "* not supported by this type"
    signum (IntegerValue x) = (DoubleValue (signum (fromInteger x)))
    signum (DoubleValue x) = (DoubleValue (signum x))
    signum _ = InvalidExpression "signum not supported for this type"
    fromInteger x = (IntegerValue x)
    abs (IntegerValue x) = (IntegerValue (abs x))
    abs (DoubleValue x) = (DoubleValue (abs x))
    abs _ = InvalidExpression "abs not supported for this type"

instance Fractional Expression where
    (/) (IntegerValue x) (IntegerValue y) = DoubleValue ((Prelude./) (fromInteger x) (fromInteger y))
    (/) (DoubleValue x) (DoubleValue y) = DoubleValue ((Prelude./) x y)
    (/) (IntegerValue x) (DoubleValue y) = DoubleValue ((Prelude./) (fromInteger x) y)
    (/) (DoubleValue x) (IntegerValue y) = DoubleValue ((Prelude./) x (fromInteger y))
    (/) _ _ = InvalidExpression "/ not supported by this type"
    recip expression = 1 / expression

instance Eq Expression where
    (/=) leftExpression rightExpression = not (leftExpression == rightExpression)
    (==) (IntegerValue x) (IntegerValue y) =  ((Prelude.==) x y)
    (==) (DoubleValue x) (DoubleValue y) =  ((Prelude.==) x y)
    (==) (IntegerValue x) (DoubleValue y) =  ((Prelude.==) (fromInteger x) y)
    (==) (DoubleValue x) (IntegerValue y) =  ((Prelude.==) x (fromInteger y))
    (==) (StringValue x) (StringValue y) = ((Prelude.==) x y)
    (==) (BoolValue x) (BoolValue y) = ((Prelude.==) x y)
    (==) (CharValue x) (CharValue y) = ((Prelude.==) x y)
    (==) (ListValue x) (ListValue y) = all ((Prelude.==) True) (zipWith (==) x y)
    (==) (TupleValue x) (TupleValue y) = ((==) (fst x) (fst y)) && ((==) (snd x) (snd y))
    (==) x y  = False

instance Ord Expression where
    compare leftExpression rightExpression
         | leftExpression == rightExpression    =  EQ
         | leftExpression <= rightExpression    =  LT
         | otherwise =  GT

    (<=) (IntegerValue x) (IntegerValue y) =  ((Prelude.<=) x y)
    (<=) (DoubleValue x) (DoubleValue y) =  ((Prelude.<=) x y)
    (<=) (IntegerValue x) (DoubleValue y) =  ((Prelude.<=) (fromInteger x) y)
    (<=) (DoubleValue x) (IntegerValue y) =  ((Prelude.<=) x (fromInteger y))
    (<=) (StringValue x) (StringValue y) = ((Prelude.<=) x y)
    (<=) (BoolValue x) (BoolValue y) = ((Prelude.<=) x y)
    (<=) (CharValue x) (CharValue y) = ((Prelude.<=) x y)
    (<=) (ListValue x) (ListValue y) = ((Prelude.<=) x y)
    (<=) (TupleValue x) (TupleValue y) = ((Prelude.<=) x y)
    (<=) x y  = False
    (<) leftExpression rightExpression =  compare leftExpression rightExpression == LT
    (>=) leftExpression rightExpression =  compare leftExpression rightExpression /= LT
    (>) leftExpression rightExpression =  compare leftExpression rightExpression == GT

instance Show Expression where
  show (StringValue x) = x
  show (IntegerValue x) = show x
  show (DoubleValue x) = show x
  show (BoolValue x) = show x
  show (CharValue x) = show x
  show (ListValue x) = show x
  show (TupleValue x) = show x
  show (BindingReference x) = x
  show (InvalidExpression reason) = "(Invalid Expression: " ++ reason ++ ")"
  show (OperatorApplication (leftExpression) operator (rightExpression)) = "(" ++ show leftExpression ++ " " ++ operator ++ " " ++ show rightExpression ++ ")"

{-Function Application-}
expressionToBool :: Expression -> Bool
expressionToBool (BoolValue x) = x

expressionToInteger :: Expression -> Integer
expressionToInteger (IntegerValue x) = x
{-ToDo: Add More-}

applyOneArgument :: (a -> b) -> FunctionArgument a -> b
applyOneArgument function (unwrap, expression) = (function (unwrap expression))

applyTwoArguments :: (a -> b -> c) -> FunctionArgument a -> FunctionArgument b -> c
applyTwoArguments  function firstArgument secondArgument = applyOneArgument (applyOneArgument function firstArgument) secondArgument

applyThreeArguments :: (a -> b -> c -> d) -> FunctionArgument a -> FunctionArgument b -> FunctionArgument c -> d
applyThreeArguments function firstArgument secondArgument thirdArgument = applyOneArgument (applyTwoArguments function firstArgument secondArgument) thirdArgument

{-Functions for  Reduction-}
solve :: [Binding] -> Expression -> Expression --reduce as much as possible
solve bindings (ListValue x) = (ListValue (map (solve bindings) x))
solve bindings (TupleValue (x, y)) = (TupleValue ((solve bindings x),(solve bindings x)))
solve bindings (BindingReference reference) = solve bindings (findBinding reference bindings)
solve bindings (OperatorApplication leftExpression operator rightExpression) = (applyOperator bindings operator leftExpression rightExpression)
solve bindings anyValue = anyValue

applyOperator :: [Binding] -> String -> Expression -> Expression -> Expression
applyOperator bindings operator leftExpression rightExpression  | (operator == "+") = (+) (solve bindings leftExpression) (solve bindings rightExpression)
                                                                | (operator == "-") = (-) (solve bindings leftExpression) (solve bindings rightExpression)
                                                                | (operator == "*") = (*) (solve bindings leftExpression) (solve bindings rightExpression)
                                                                | (operator == "/") = (/) (solve bindings leftExpression) (solve bindings rightExpression)
                                                                | (operator == "<") = (BoolValue ((<) (solve bindings leftExpression) (solve bindings rightExpression)))
                                                                | (operator == "<=") = (BoolValue ((<=) (solve bindings leftExpression) (solve bindings rightExpression)))
                                                                | (operator == ">") = (BoolValue ((>) (solve bindings leftExpression) (solve bindings rightExpression)))
                                                                | (operator == ">=") = (BoolValue ((>=) (solve bindings leftExpression) (solve bindings rightExpression)))
                                                                | (operator == "==") = (BoolValue ((>=) (solve bindings leftExpression) (solve bindings rightExpression)))
                                                                | (operator == "||") =  (BoolValue (applyTwoArguments (||) (expressionToBool, (solve bindings leftExpression)) (expressionToBool, (solve bindings rightExpression))))
                                                                | (operator == "&&") =  (BoolValue (applyTwoArguments (&&) (expressionToBool, (solve bindings leftExpression)) (expressionToBool, (solve bindings rightExpression))))
                                                                | otherwise = InvalidExpression "Unknown Operator"

findBinding :: String -> [Binding] -> Expression
findBinding key bindings = (tryFindBinding key bindings) ?? (InvalidExpression "Binding not Found")

tryFindBinding :: String -> [Binding] -> Maybe Expression
tryFindBinding key bindings = do
    foundBinding <- find (\ (x, _) -> x == key) bindings
    return (snd foundBinding)

-- see https://stackoverflow.com/questions/47371950/maybe-coalescing-operator (copied code)
infixr 3 ??
(??) :: Maybe a -> a -> a
Just x ?? _ = x
Nothing ?? y = y


canBeReduced :: Expression -> Bool
canBeReduced (OperatorApplication _ _ _) = True --later we have to consider partial application here!
canBeReduced (BindingReference _) = True
canBeReduced _ = False --Notice: Tuples, List etc return false, this allows us to have expressions like [1/2, 1/0]

applyStep :: [Binding] -> Expression -> (ReductionStepDescription, Expression)
applyStep bindings (BindingReference reference) = (("Replace '" ++ reference ++ "' with definition"),(findBinding reference bindings)) {-replace binding reference with actual expression (Delta Reduction)-}
applyStep bindings (OperatorApplication leftExpression operator rightExpression)    | canBeReduced leftExpression = do
                                                                                        let (reductionStep, reducedLeftExpression) = (applyStep bindings leftExpression )
                                                                                        ({-"Reduce Left-Hand-Side-Expression of Opperator-Application: " ++ -}reductionStep, (OperatorApplication reducedLeftExpression operator rightExpression))
                                                                                    | canBeReduced rightExpression = do
                                                                                        let (reductionStep, reducedRightExpression) = (applyStep bindings rightExpression )
                                                                                        ({-"Reduce Right-Hand-Side-Expression of Opperator-Application: " ++ -}reductionStep, (OperatorApplication leftExpression operator reducedRightExpression))
                                                                                    | otherwise = (("Apply Operator " ++ operator),(applyOperator bindings operator leftExpression rightExpression))
applyStep _ x | canBeReduced x = ("no reduction rule implemented for this expression", x)
              | otherwise = ("is already reduced", x)



printBindings :: [Binding] -> IO ()
printBindings [] = return ()
printBindings ((reference, expression):xs) = do
    putStrLn (reference ++ " = " ++ show expression)
    printBindings xs

{-Printing-}
printStepByStepReduction :: [Binding] -> Expression -> IO ()
printStepByStepReduction bindings expression    | canBeReduced expression = do
                                                    print expression
                                                    let (reductionStepDescription, reducedExpression) = (applyStep bindings expression)
                                                    putStrLn ("{-" ++ reductionStepDescription ++ "-}")
                                                    printStepByStepReduction bindings reducedExpression
                                                | otherwise = print expression