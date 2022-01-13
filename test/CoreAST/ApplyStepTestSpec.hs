{-|
Module      : ApplyStepTestSpec
Description : Hspec tests for the stepper
License     : GPL-3

Each tests loads an expressions defined in "IntegrationTestBindingsForApplyStepTest.hs".
For each expression, the function "apply step" is applied. Then
it is checked if the resulting "reduced expression" is as expected. Also 
each reduction reduces a "step description" which is checked too
-}
module CoreAST.ApplyStepTestSpec where

import DataProvider.DataProvider
  ( getBindingFinderWithCoreBindings,
  )
import GHC.Plugins
import CoreAST.PrettyPrinter(toHaskellLikeString, prettyPrintToOriginalHaskellCoreString)
import CoreAST.Stepper (applyStep)
import CoreAST.Helpers.TraceHelper
import CoreAST.TypeDefinitions

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
spec = beforeAll (getBindingFinderWithCoreBindings "test/ApplyStepBindings.hs") $ do
  describe "Apply Step" $ do
    it "apply step to function reference var" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = bindingFinder "functionReference"
      let (reductionStepDescription, reducedExpression, _) = fromJust (applyStep coreBindings originalExpression)
      reducedExpression `shouldSatisfy` isALamda
      reductionStepDescription `shouldSatisfy` isDeltaReductionStep
    it "apply step to lamda application" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = bindingFinder "lamdaApplication"
      let (_, reducedExpression, _) = fromJust (applyStep coreBindings originalExpression)
      let (_, twoTimesreducedExpression, _) = fromJust (applyStep coreBindings reducedExpression)
      let (thirdReductionStepDescription, threeTimesreducedExpression, _) = fromJust (applyStep coreBindings twoTimesreducedExpression)
      threeTimesreducedExpression `shouldSatisfy` isLiteral
      thirdReductionStepDescription `shouldSatisfy` isApplicationStep      
    it "apply step to case" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = bindingFinder "caseWithNonReducedExpression"
      let (_, reducedExpression, _) = fromJust (applyStep coreBindings originalExpression)
      let (secondReductionStepDescription, reducedTwiceExpression, _) = fromJust (applyStep coreBindings reducedExpression)
      reducedTwiceExpression `shouldSatisfy` isCase
      secondReductionStepDescription `shouldSatisfy` isCaseExpressionStep   
      let (thirdReductionStepDescription, reducedThreeTimesExpression, _) = fromJust (applyStep coreBindings reducedTwiceExpression)
      reducedThreeTimesExpression `shouldSatisfy` isVar
      thirdReductionStepDescription `shouldSatisfy` isPatternMatchStep   
    it "apply step to non-recursive let" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = bindingFinder "nonRecursiveLetExpression"
      let (firstReductionStepDescription, reducedExpression, _) = fromJust (applyStep coreBindings originalExpression)
      let (secondReductionStepDescription, secondreducedExpression, _) = fromJust (applyStep coreBindings reducedExpression)
      reducedExpression `shouldSatisfy` isApp
      secondreducedExpression `shouldSatisfy` isLiteral
      firstReductionStepDescription `shouldSatisfy` isStrictApplicationArgumentStep  
      secondReductionStepDescription `shouldSatisfy` isEvaluationStep   
    it "apply step to recursive let" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = bindingFinder "recursiveLetExpression"
      let (reductionStepDescription, reducedExpression, newBindings) = fromJust (applyStep coreBindings originalExpression)
      reducedExpression `shouldSatisfy` isApp
      reductionStepDescription `shouldSatisfy` isReplaceLetStep         
      length coreBindings + 1 `shouldBe` length newBindings
    it "apply step to nested unsteppable expression" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = bindingFinder "nestedUnsteppableExpression"
      let (reductionStepDescription, reducedExpression, newBindings) = fromJust (applyStep coreBindings originalExpression)
      reducedExpression `shouldSatisfy` isApp
      reductionStepDescription `shouldSatisfy` isStrictApplicationArgumentStep         
    it "evaluate unsteppable expression" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = bindingFinder "unsteppableExpression"
      let (reductionStepDescription, reducedExpression, newBindings) = fromJust (applyStep coreBindings originalExpression)
      reducedExpression `shouldSatisfy` isLiteral
      reductionStepDescription `shouldSatisfy` isEvaluationStep      
    it "apply step to expression with reducable function" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = bindingFinder "expressionWithReducableFunction"
      let (reductionStepDescription, reducedExpression, newBindings) = fromJust (applyStep coreBindings originalExpression)
      reducedExpression `shouldSatisfy` isApp
      reductionStepDescription `shouldSatisfy` isApplicationExpressionStep    
    it "apply step to expression with function from class dictionary" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = bindingFinder "applicationWithFunctionFromClassDictionary"
      let (reductionStepDescription, reducedExpression, newBindings) = fromJust (applyStep coreBindings originalExpression)
      reducedExpression `shouldSatisfy` isApp
      reductionStepDescription `shouldSatisfy` isClassDictionaryLookupStep    
    it "does not apply step to fully reduced expression" $ \(bindingFinder, coreBindings) -> do
      let originalExpression = bindingFinder "fullyReducedExpression"
      let result = applyStep coreBindings originalExpression
      isNothing result `shouldBe` True  

  where      

    {-Helper predicate functions -}

    isALamda :: CoreExpr -> Bool
    isALamda Lam {} = True
    isALamda _ = False

    isLiteral :: CoreExpr -> Bool
    isLiteral Lit {} = True
    isLiteral _ = False

    isCase :: CoreExpr -> Bool
    isCase Case {} = True
    isCase _ = False

    isVar :: CoreExpr -> Bool
    isVar Var {} = True
    isVar _ = False

    isApp :: CoreExpr -> Bool
    isApp App {} = True
    isApp _ = False

    isDeltaReductionStep :: ReductionStepDescription -> Bool
    isDeltaReductionStep DeltaReductionStep {} = True
    isDeltaReductionStep _ = False

    isApplicationStep :: ReductionStepDescription -> Bool
    isApplicationStep ApplicationStep {} = True
    isApplicationStep _ = False

    isEvaluationStep :: ReductionStepDescription -> Bool
    isEvaluationStep EvaluationStep {} = True
    isEvaluationStep _ = False

    isCaseExpressionStep :: ReductionStepDescription -> Bool
    isCaseExpressionStep (NestedReduction (CaseExpressionStep : _)) = True
    isCaseExpressionStep _ = False

    isPatternMatchStep :: ReductionStepDescription -> Bool
    isPatternMatchStep PatternMatchStep {} = True
    isPatternMatchStep _ = False

    isReplaceLetStep :: ReductionStepDescription -> Bool
    isReplaceLetStep ReplaceLetStep {} = True
    isReplaceLetStep (NestedReduction reductions) = isReplaceLetStep (last reductions)
    isReplaceLetStep _ = False

    isApplicationExpressionStep :: ReductionStepDescription -> Bool
    isApplicationExpressionStep (NestedReduction (ApplicationExpressionStep : _)) = True
    isApplicationExpressionStep _ = False

    isClassDictionaryLookupStep :: ReductionStepDescription -> Bool
    isClassDictionaryLookupStep ClassDictionaryLookupStep {} = True
    isClassDictionaryLookupStep _ = False

    isStrictApplicationArgumentStep :: ReductionStepDescription -> Bool
    isStrictApplicationArgumentStep (NestedReduction (StrictApplicationArgumentStep : _)) = True
    isStrictApplicationArgumentStep _ = False