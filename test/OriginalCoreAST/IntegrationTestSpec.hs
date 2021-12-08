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
type Binding = (Var, Expr Var)



spec :: Spec
spec = before getBindingFinderWithCoreBindings $ do
    describe "Arithmetic" $ do
        it "can reduce adition" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "addition" bindingFinder coreBindings

expectationForExpression :: String -> (String -> Expr Var) -> [Binding] -> Expectation
expectationForExpression expressionBindingName bindingFinder coreBindings = do
        let input = (bindingFinder (expressionBindingName ++ "Input"))
        let expectedOutput = (bindingFinder(expressionBindingName ++ "ExpectedOutput"))
        let output = reduceToHeadNormalForm coreBindings input
        output `shouldBe` expectedOutput