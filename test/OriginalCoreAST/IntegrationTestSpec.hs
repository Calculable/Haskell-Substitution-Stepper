module OriginalCoreAST.IntegrationTestSpec where
    
import Test.Hspec
import Test.Hspec.QuickCheck
import DataProvider.DataProvider(getBindingFinderWithCoreBindings)

import OriginalCoreAST.CoreInformationExtractorFunctions()
import Utils (showOutputable)
import GHC.Plugins (liftIO )
import GHC.Core (Bind (NonRec), CoreProgram, Expr)
import GHC.Types.Var (Var (varName, varType))
import OriginalCoreAST.CoreStepperHelpers.CoreLookup(findBindingForString)
import OriginalCoreAST.CoreStepper(reduceToHeadNormalForm)
import OriginalCoreAST.CoreStepperHelpers.CoreEvaluator(prepareExpressionArgumentForEvaluation)
import Control.Exception (evaluate)
import Control.DeepSeq (force)

type Binding = (Var, Expr Var)



spec :: Spec
spec = before getBindingFinderWithCoreBindings $ do
    describe "Arithmetic Operators" $ do
        it "can reduce adition" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "addition" bindingFinder coreBindings
 

expectationForExpression :: String -> (String -> Expr Var) -> [Binding] -> Expectation
expectationForExpression expressionBindingName bindingFinder coreBindings = do
        let input = (bindingFinder (expressionBindingName ++ "Input"))
        let expectedOutput = (bindingFinder(expressionBindingName ++ "ExpectedOutput"))
        let output = reduceToHeadNormalForm coreBindings input
        (prepareExpressionArgumentForEvaluation output) `shouldBe` (prepareExpressionArgumentForEvaluation expectedOutput)

