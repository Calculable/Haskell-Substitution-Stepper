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
module OriginalCoreAST.ApplyStepTestSpec where

import DataProvider.DataProvider
  ( getBindingFinderWithCoreBindings,
  )
import GHC.Plugins
import OriginalCoreAST.CorePrettyPrinter(toHaskellLikeString, prettyPrintToOriginalHaskellCoreString)
import OriginalCoreAST.CoreStepper (applyStep)
import OriginalCoreAST.CoreStepperHelpers.CoreTracerHelper
import OriginalCoreAST.CoreTypeDefinitions

import Data.Maybe


import Test.Hspec
  ( Expectation,
    Spec,
    beforeAll,
    describe,
    it,
    pendingWith,
    shouldBe,
    shouldSatisfy
  )

type Binding = (Var, Expr Var)

spec :: Spec
spec = beforeAll (getBindingFinderWithCoreBindings "src/IntegrationTestBindingsForApplyStepTest.hs") $ do
  describe "Apply Step" $ do
    it "apply step to function reference var" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = traceExpression "original expression: " (bindingFinder "functionReference")
      let (reductionStepDescription, reductedExpression, newBindings) = fromJust (applyStep coreBindings originalExpression)
      traceExpression "reduced expression" reductedExpression `shouldSatisfy` isALamda
      reductionStepDescription `shouldSatisfy` isDeltaReduction
      
        

isALamda :: CoreExpr -> Bool
isALamda (Lam _ _) = True
isALamda _ = False

isDeltaReduction :: ReductionStepDescription -> Bool
isDeltaReduction (DeltaReductionStep _) = True
isDeltaReduction _ = False