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

type Binding = (Var, Expr Var)



spec :: Spec
spec = before getBindingFinderWithCoreBindings $ do
    describe "Arithmetic" $ do
        it "can reduce adition" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "addition" bindingFinder coreBindings
        it "can reduce adition with application syntax" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "additionWithApplicationSyntax" bindingFinder coreBindings
        it "can reduce substraction" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "substraction" bindingFinder coreBindings
        it "can reduce nested arithmetic" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "nestedArithmetic" bindingFinder coreBindings
        it "can reduce double addition" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "doubleAddition" bindingFinder coreBindings
    describe "Basic Function/Lamda Application" $ do   
        it "can reduce function application" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "functionApplication" bindingFinder coreBindings
        it "can reduce lamda application" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "lamdaApplication" bindingFinder coreBindings
        it "can reduce nested application" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "nestedApplication" bindingFinder coreBindings
    describe "Higher Order Function/Lamda Application" $ do     
        it "can reduce function application with higher order result" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "higherOrderResult" bindingFinder coreBindings
        it "can reduce function application with higher order parameter" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "higherOrderParameter" bindingFinder coreBindings
    describe "Types" $ do        
        it "can reduce basic operation on Boolean" $ \(bindingFinder, coreBindings) -> do
            --expectationForExpression "basicOperationOnBoolean" bindingFinder coreBindings
            pendingWith "Boolean type not fully supported"
        it "can reduce basic operation on Char" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "basicOperationOnChar" bindingFinder coreBindings
        it "can reduce basic operation on Double" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "basicOperationOnDouble" bindingFinder coreBindings
        it "can reduc basic operation on Int" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "basicOperationOnInt" bindingFinder coreBindings
        it "can reduce basic operation on Integer" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "basicOperationOnInteger" bindingFinder coreBindings
        it "can reduce basic operation on Either" $ \(bindingFinder, coreBindings) -> do
            --expectationForExpression "basicOperationOnEither" bindingFinder coreBindings
            pendingWith "Either type not fully supported"
        it "can reduce basic operation on Maybe" $ \(bindingFinder, coreBindings) -> do
            --expectationForExpression "basicOperationOnMaybe" bindingFinder coreBindings
            pendingWith "Maybe type not fully supported"
        it "can reduce basic operation on  Rational" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "basicOperationOnRational" bindingFinder coreBindings
        it "can reduce basic operation on String" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "basicOperationOnString" bindingFinder coreBindings
        it "can reduce basic operation on List" $ \(bindingFinder, coreBindings) -> do
            --expectationForExpression "basicOperationOnList" bindingFinder coreBindings
            pendingWith "List type not fully supported"
        it "can reduce basic operation on Tuple" $ \(bindingFinder, coreBindings) -> do
            --expectationForExpression "basicOperationOnTuple" bindingFinder coreBindings
            pendingWith "Tuple type not fully supported"


expectationForExpression :: String -> (String -> Expr Var) -> [Binding] -> Expectation
expectationForExpression expressionBindingName bindingFinder coreBindings = do
        let input = (bindingFinder (expressionBindingName ++ "Input"))
        let expectedOutput = (bindingFinder(expressionBindingName ++ "ExpectedOutput"))
        let output = reduceToHeadNormalForm coreBindings input
        (prepareExpressionArgumentForEvaluation output) `shouldBe` (prepareExpressionArgumentForEvaluation expectedOutput)


