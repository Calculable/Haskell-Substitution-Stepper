{-# OPTIONS -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

{-|
Module      : IntegrationTestBindingsForPrettyPrinter
Description : Contains functions and expressions used in integration tests for the pretty printer
License     : GPL-3

-}
module PrettyPrinterBindings where

import SteppablePrelude

multiArgumentLamda = \x y -> x + y

list = [1, 2, 3]

tuple = (1, 2, 3)

caseWithLocalBindingsExpression = case [1, 2] ++ [3] of {
  [] -> True;
  (x:xs) -> False
}

nestedApplication = abs (negate 5)

functionApplication = abs 3

operatorApplication = (+) 1 2