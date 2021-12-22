{-|
Module      : IntegrationTestSpec
Description : Hspec integration tests
License     : GPL-3

Each tests loads two expressions defined in "IntegrationTestBindings.hs", an
Input-Expression and an Exprected-Output-Expression.
The "Input" binding is reduced to normal form using the CoreStepper.
Finally, there is an equality check, if the reduced Input-Expression equals the defined
"ExpectedOutput" expression. If this is not the case, the test fails.
-}
module OriginalCoreAST.CorePrettyPrinterTestSpec where

import DataProvider.DataProvider
  ( getBindingFinderWithCoreBindings,
  )
import GHC.Plugins (Expr, Var)
import OriginalCoreAST.CorePrettyPrinter(toHaskellLikeString, prettyPrintToOriginalHaskellCoreString)

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
spec = beforeAll (getBindingFinderWithCoreBindings "src/IntegrationTestBindingsForPrettyPrinterTest.hs") $ do
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
