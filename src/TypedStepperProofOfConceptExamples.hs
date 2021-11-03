module TypedStepperProofOfConceptExamples
  (printExampleStepping
  )
where

import TypedStepperProofOfConcept

{-Examples-}

exampleBindings = [
     ("w", (DoubleValue 6)), {-Represents the expression "x = 1+2*3"-}
     ("x", OperatorApplication ( (IntegerValue 1)) "+" ( (OperatorApplication ( (IntegerValue 2)) "*" ( (IntegerValue 3))))), {-Represents the expression "x = 1+2*3"-}
     ("y", OperatorApplication ( (IntegerValue 3)) "-" ( (IntegerValue 4))), {-Represents the expression "y = 3-4"-}
     ("z", OperatorApplication ( (BindingReference "x")) "+" ( (BindingReference "y"))) {-Represents the expression "x + y"-}
     ]

exampleExpression1 = OperatorApplication (BindingReference "z") "==" (BindingReference "w")
exampleExpression2 = OperatorApplication (TupleValue ((IntegerValue 1), ( (OperatorApplication ( (IntegerValue 1)) "+" ( (IntegerValue 2)))))) "==" (TupleValue ((IntegerValue 1), ( (OperatorApplication ( (IntegerValue 2)) "+" ( (IntegerValue 1))))))
exampleExpression3 = OperatorApplication (BoolValue True) "&&" (BoolValue False)

printExampleStepping :: IO ()
printExampleStepping = do

    putStrLn "\nBindings"
    printBindings exampleBindings

    putStrLn "\n\nExample-Expression 1"
    print exampleExpression1

    putStrLn "\nReduction 1"
    printStepByStepReduction exampleBindings exampleExpression1

    putStrLn "\n\nExample-Expression 2"
    print exampleExpression2

    putStrLn "\nReduction 2"
    printStepByStepReduction exampleBindings exampleExpression2

    putStrLn "\n\nExample-Expression 3"
    print exampleExpression3

    putStrLn "\nReduction 3"
    printStepByStepReduction exampleBindings exampleExpression3

