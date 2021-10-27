module TypedStepperProofOfConcept
  (printExampleStepping
  )
where

import Data.List
import Data.Bool

type Operator = String
type ReductionStepDescription = String --for example: "replace x with definition"
type Binding = (String, Expression)

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

instance Num Expression where
    (+) leftExpression rightExpression = add leftExpression rightExpression
    (*) leftExpression rightExpression = multiplicate leftExpression rightExpression
    (-) leftExpression rightExpression = minus leftExpression rightExpression
    signum (IntegerValue x) = (DoubleValue (signum (fromInteger x)))
    signum (DoubleValue x) = (DoubleValue (signum x))
    signum _ = InvalidExpression "signum not supported for this type"
    fromInteger x = (IntegerValue x)
    abs (IntegerValue x) = (IntegerValue (abs x))
    abs (DoubleValue x) = (DoubleValue (abs x))
    abs _ = InvalidExpression "abs not supported for this type"

instance Fractional Expression where
    (/) leftExpression rightExpression = divide leftExpression rightExpression
    recip expression = 1 / expression

instance Eq Expression where
    (/=) leftExpression rightExpression = not (leftExpression == rightExpression)
    (==) leftExpression rightExpression = equals leftExpression rightExpression

instance Ord Expression where
    compare leftExpression rightExpression
         | leftExpression == rightExpression    =  EQ
         | leftExpression <= rightExpression    =  LT
         | otherwise =  GT

    (<=) leftExpression rightExpression =  lessThenOrEqual leftExpression rightExpression
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



-- toDo: Make more generic
expressionOr :: Expression -> Expression -> Expression
expressionOr (BoolValue x) (BoolValue y) = (BoolValue (x || y))
expressionOr _ _ = InvalidExpression "|| not supported fot this type"

expressionAnd :: Expression -> Expression -> Expression
expressionAnd (BoolValue x) (BoolValue y) = (BoolValue (x && y))
expressionAnd _ _ = InvalidExpression "&& not supported fot this type"

-- i found no way no make this operator definitions less redundant - if someone has a better idea i would be
-- very interested to hear it :)
-- unforunately we can't make a function apply operator where we pass the operator as an argument
-- because if the operator expects a Num, we cant write "customoperator myDoubleValue" as that would be an error
lessThenOrEqual :: Expression -> Expression -> Bool
lessThenOrEqual (IntegerValue x) (IntegerValue y) =  ((Prelude.<=) x y)
lessThenOrEqual (DoubleValue x) (DoubleValue y) =  ((Prelude.<=) x y)
lessThenOrEqual (IntegerValue x) (DoubleValue y) =  ((Prelude.<=) (fromInteger x) y)
lessThenOrEqual (DoubleValue x) (IntegerValue y) =  ((Prelude.<=) x (fromInteger y))
lessThenOrEqual (StringValue x) (StringValue y) = ((Prelude.<=) x y)
lessThenOrEqual (BoolValue x) (BoolValue y) = ((Prelude.<=) x y)
lessThenOrEqual (CharValue x) (CharValue y) = ((Prelude.<=) x y)
lessThenOrEqual (ListValue x) (ListValue y) = ((Prelude.<=) x y)
lessThenOrEqual (TupleValue x) (TupleValue y) = ((Prelude.<=) x y)
lessThenOrEqual x y  = False

equals :: Expression -> Expression -> Bool
equals (IntegerValue x) (IntegerValue y) =  ((Prelude.==) x y)
equals (DoubleValue x) (DoubleValue y) =  ((Prelude.==) x y)
equals (IntegerValue x) (DoubleValue y) =  ((Prelude.==) (fromInteger x) y)
equals (DoubleValue x) (IntegerValue y) =  ((Prelude.==) x (fromInteger y))
equals (StringValue x) (StringValue y) = ((Prelude.==) x y)
equals (BoolValue x) (BoolValue y) = ((Prelude.==) x y)
equals (CharValue x) (CharValue y) = ((Prelude.==) x y)
equals (ListValue x) (ListValue y) = all ((Prelude.==) True) (zipWith equals x y)
equals (TupleValue x) (TupleValue y) = (equals (fst x) (fst y)) && (equals (snd x) (snd y))
equals x y  = False


add :: Expression -> Expression -> Expression
add (IntegerValue x) (IntegerValue y) = IntegerValue ((Prelude.+) x y)
add (DoubleValue x) (DoubleValue y) = DoubleValue ((Prelude.+) x y)
add (IntegerValue x) (DoubleValue y) = DoubleValue ((Prelude.+) (fromInteger x) y)
add (DoubleValue x) (IntegerValue y) = DoubleValue ((Prelude.+) x (fromInteger y))
add _ _ = InvalidExpression "+ not supported by this type"

minus :: Expression -> Expression -> Expression
minus (IntegerValue x) (IntegerValue y) = IntegerValue ((Prelude.-) x y)
minus (DoubleValue x) (DoubleValue y) = DoubleValue ((Prelude.-) x y)
minus (IntegerValue x) (DoubleValue y) = DoubleValue ((Prelude.-) (fromInteger x) y)
minus (DoubleValue x) (IntegerValue y) = DoubleValue ((Prelude.-) x (fromInteger y))
minus _ _ = InvalidExpression "- not supported by this type"

multiplicate :: Expression -> Expression -> Expression
multiplicate (IntegerValue x) (IntegerValue y) = IntegerValue ((Prelude.*) x y)
multiplicate (DoubleValue x) (DoubleValue y) = DoubleValue ((Prelude.*) x y)
multiplicate (IntegerValue x) (DoubleValue y) = DoubleValue ((Prelude.*) (fromInteger x) y)
multiplicate (DoubleValue x) (IntegerValue y) = DoubleValue ((Prelude.*) x (fromInteger y))
multiplicate _ _ = InvalidExpression "* not supported by this type"

divide :: Expression -> Expression -> Expression
divide (IntegerValue x) (IntegerValue y) = DoubleValue ((Prelude./) (fromInteger x) (fromInteger y))
divide (DoubleValue x) (DoubleValue y) = DoubleValue ((Prelude./) x y)
divide (IntegerValue x) (DoubleValue y) = DoubleValue ((Prelude./) (fromInteger x) y)
divide (DoubleValue x) (IntegerValue y) = DoubleValue ((Prelude./) x (fromInteger y))
divide _ _ = InvalidExpression "/ not supported by this type"


solve :: [Binding] -> Expression -> Expression
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
                                                                | (operator == "||") =  (expressionOr (solve bindings leftExpression) (solve bindings rightExpression))
                                                                | (operator == "&&") = (expressionAnd (solve bindings leftExpression) (solve bindings rightExpression))
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

printBindings :: [Binding] -> IO ()
printBindings [] = return ()
printBindings ((reference, expression):xs) = do
    putStrLn (reference ++ " = " ++ show expression)
    printBindings xs


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

printStepByStepReduction :: [Binding] -> Expression -> IO ()
printStepByStepReduction bindings expression    | canBeReduced expression = do
                                                    print expression
                                                    let (reductionStepDescription, reducedExpression) = (applyStep bindings expression)
                                                    putStrLn ("{" ++ reductionStepDescription ++ "}")
                                                    printStepByStepReduction bindings reducedExpression
                                                | otherwise = print expression


exampleBindings = [
     ("w", (DoubleValue 6)), {-Represents the expression "x = 1+2*3"-}
     ("x", OperatorApplication ( (IntegerValue 1)) "+" ( (OperatorApplication ( (IntegerValue 2)) "*" ( (IntegerValue 3))))), {-Represents the expression "x = 1+2*3"-}
     ("y", OperatorApplication ( (IntegerValue 3)) "-" ( (IntegerValue 4))), {-Represents the expression "y = 3-4"-}
     ("z", OperatorApplication ( (BindingReference "x")) "+" ( (BindingReference "y"))) {-Represents the expression "x + y"-}
     ]

--exampleExpression = OperatorApplication (BindingReference "z") "==" (BindingReference "w")


exampleExpression = OperatorApplication (TupleValue ((IntegerValue 1), ( (OperatorApplication ( (IntegerValue 1)) "+" ( (IntegerValue 2)))))) "==" (TupleValue ((IntegerValue 1), ( (OperatorApplication ( (IntegerValue 2)) "+" ( (IntegerValue 1))))))

printExampleStepping :: IO ()
printExampleStepping = do

    putStrLn "\nBindings"
    printBindings exampleBindings

    putStrLn "\nExpression to Step"
    print exampleExpression

    putStrLn "\nReduction"
    printStepByStepReduction exampleBindings exampleExpression
