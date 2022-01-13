{-|
Module      : CorePrettyPrinterTestSpec
Description : Hspec tests for the pretty printer
License     : GPL-3

Each tests loads an expressions defined in "IntegrationTestBindingsForPrettyPrinterTest.hs".
The "Input" expression is then shown as a string and compared with the expected result.
-}
module CoreAST.CorePrettyPrinterTestSpec where

import DataProvider.DataProvider
  ( getBindingFinderWithCoreBindings,
  )
import GHC.Plugins (Expr, Var)
import CoreAST.PrettyPrinter(toHaskellLikeString, prettyPrintToOriginalHaskellCoreString)

import Test.Hspec
  ( Expectation,
    Spec,
    beforeAll,
    describe,
    it,
    pendingWith,
    shouldBe,
  )

type Binding = (Var, Expr Var)

spec :: Spec
spec = beforeAll (getBindingFinderWithCoreBindings "test/PrettyPrinterBindings.hs") $ do
  describe "Pretty Print like Haskell" $ do
    it "pretty-prints multi argument lamda" $ \(bindingFinder, coreBindings) -> do
      let expressionString = toHaskellLikeString False (bindingFinder "multiArgumentLamda")
      expressionString `shouldBe` "\\x y -> x + y"
    it "pretty-prints list" $ \(bindingFinder, coreBindings) -> do
      let expressionString = toHaskellLikeString False (bindingFinder "list")
      expressionString `shouldBe` "[1, 2, 3]"
    it "pretty-prints tuple" $ \(bindingFinder, coreBindings) -> do
      let expressionString = toHaskellLikeString False (bindingFinder "tuple")
      expressionString `shouldBe` "(1, 2, 3)"
    it "pretty-prints case statement with local bindings" $ \(bindingFinder, coreBindings) -> do
      let expressionString = toHaskellLikeString False (bindingFinder "caseWithLocalBindingsExpression")
      expressionString `shouldBe` "case [1, 2] ++ [3] of {\n  [] -> True;\n  : x xs -> False\n}"
    it "pretty-prints function application" $ \(bindingFinder, coreBindings) -> do
      let expressionString = toHaskellLikeString False (bindingFinder "functionApplication")
      expressionString `shouldBe` "abs 3"
    it "pretty-prints infix operator application" $ \(bindingFinder, coreBindings) -> do
      let expressionString = toHaskellLikeString False (bindingFinder "operatorApplication")
      expressionString `shouldBe` "1 + 2"
    it "pretty-prints nested application" $ \(bindingFinder, coreBindings) -> do
      let expressionString = toHaskellLikeString False (bindingFinder "nestedApplication")
      expressionString `shouldBe` "abs (negate 5)"
    it "can optionally show types" $ \(bindingFinder, coreBindings) -> do
      let expressionString = toHaskellLikeString True (bindingFinder "list")
      expressionString `shouldBe` "[@Integer, 1, @Integer, 2, @Integer, 3]"
  describe "Pretty Print like Core" $ do
    it "can pretty print Core expressions" $ \(bindingFinder, coreBindings) -> do
      let expressionString = prettyPrintToOriginalHaskellCoreString (bindingFinder "list")
      expressionString `shouldBe` ": @Integer 1 (: @Integer 2 (: @Integer 3 ([] @Integer)))"
