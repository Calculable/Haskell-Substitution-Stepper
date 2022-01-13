{-# OPTIONS -XNoImplicitPrelude #-}

{-|
Module      : IntegrationTestBindingsForPrettyPrinter
Description : Contains functions and expressions used in integration tests for the pretty printer
License     : GPL-3

-}
module ApplyStepBindings where

import SteppablePrelude
    ( Num((+)), Bool(False, True), Int, Enum(succ), id, reverse )

lamdaApplication = id 42

functionReference = reverse

caseWithNonReducedExpression = case 1 + 1 of {
  42 -> True;
  _ -> False
}

nonRecursiveLetExpression = x + 1 where x = 1 + 1

recursiveLetExpression = x where x = x + 1

nestedUnsteppableExpression = (1 + 1) + 1

unsteppableExpression = 1 + 1

expressionWithReducableFunction = reverse [1, 2, 3]

applicationWithFunctionFromClassDictionary = succ 1::Int

fullyReducedExpression = \(x) -> x