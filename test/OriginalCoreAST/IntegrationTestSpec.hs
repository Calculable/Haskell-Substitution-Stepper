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
    describe "Support for Num Type" $ do        

        it "abs works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "abs" bindingFinder coreBindings
        it "signum works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "signum" bindingFinder coreBindings
        it "signum with double works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "signumWithDouble" bindingFinder coreBindings
    describe "Support for Fractional Type" $ do        


        it "division works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "division" bindingFinder coreBindings
        it "recip works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "recip" bindingFinder coreBindings
    describe "Support for EQ Type" $ do  
        it "equals with char works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "equalsChar" bindingFinder coreBindings
        it "equals with integer works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "equalsInteger" bindingFinder coreBindings
        it "equals with double works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "equalsDouble" bindingFinder coreBindings
        it "equals with string works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "equalsString" bindingFinder coreBindings
        it "not equals works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "notEquals" bindingFinder coreBindings
        it "equals for not equal strings works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "equalsForNotEqualString" bindingFinder coreBindings
    describe "Support for Ord Type" $ do      
        it "less or equal operator works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "lessOrEqual" bindingFinder coreBindings
        it "less operator works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "less" bindingFinder coreBindings
        it "greater or equal operator works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "greaterOrEqual" bindingFinder coreBindings
        it "max works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "max" bindingFinder coreBindings
    describe "Support for Enum Type" $ do      
        it "succ with integer works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "succWithInteger" bindingFinder coreBindings
        it "succ with double works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "succWithDouble" bindingFinder coreBindings
    describe "Support for Floating Type" $ do      
        it "exp works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "exp" bindingFinder coreBindings
        it "log works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "log" bindingFinder coreBindings
        it "sqrt works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "sqrt" bindingFinder coreBindings
        it "power works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "power" bindingFinder coreBindings
        it "cosinus works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "cosinus" bindingFinder coreBindings
    describe "Support for Integral Type" $ do  
        it "integer division works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "integerDivision" bindingFinder coreBindings
        it "modulo works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "modulo" bindingFinder coreBindings
    describe "Polymorphism" $ do  
        it "polymorphic functions work" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "polymorphicFunction" bindingFinder coreBindings
        it "polymorphic functions with type constraints work" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "polymorphicFunctionWithTypeConstraint" bindingFinder coreBindings
        it "functions with polymorphic result work" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "functionWithPolymorphicResult" bindingFinder coreBindings
    describe "Non-Strictness" $ do  
        it "function arguments are evaluated non-strict" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "nonStrictness" bindingFinder coreBindings
    describe "Pattern Matching" $ do 
        it "pattern matching on integer works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "integerPatternMatching" bindingFinder coreBindings
        it "pattern matching on string works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "stringPatternMatching" bindingFinder coreBindings
        it "pattern matching on boolean works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "booleanPatternMatching" bindingFinder coreBindings
        it "pattern matching on multiple arguments works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "multiArgumentPatternMatching" bindingFinder coreBindings
        it "pattern matching on list of integer works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "patternMatchingOnIntegerList" bindingFinder coreBindings
        it "pattern matching on list of empty list works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "patternMatchingOnEmptyList" bindingFinder coreBindings
        it "pattern matching on list of any type works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "patternMatchingOnPolymorphicList" bindingFinder coreBindings
        it "pattern matching on any constructor works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "patternMatchingOnAnyConstructor" bindingFinder coreBindings
        it "pattern matching works on unsupported type" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "patternMatchingOnUnsupportedType" bindingFinder coreBindings
    describe "Recursion" $ do 
        it "recursion works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "recursion" bindingFinder coreBindings
    describe "List Operations" $ do 
        it "list operations work" $ \(bindingFinder, coreBindings) -> do
            --expectationForExpression "listOperations" bindingFinder coreBindings
            pendingWith "not all list operations are supported yet"
    describe "Where Bindings" $ do
        it "expressions with where syntax can be reduced" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "where" bindingFinder coreBindings
        it "expressions with multiple where-bindings syntax can be reduced" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "multipleWhere" bindingFinder coreBindings
    describe "Do / Let Syntax" $ do
        it "expressions with do/let syntax can be reduced" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "functionWithDoAndLet" bindingFinder coreBindings
    describe "Tuples" $ do
        it "Tuples as parameter work" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "tupleAsParameter" bindingFinder coreBindings
        it "Equality on Tuple works" $ \(bindingFinder, coreBindings) -> do
            pendingWith "tuples are not fully supported yet"
            --expectationForExpression "equalsOnTuple" bindingFinder coreBindings               
    describe "Infinite List" $ do
        it "Infinite lists works as function parameter" $ \(bindingFinder, coreBindings) -> do
            pendingWith "lists are not fully supported yet"
            --expectationForExpression "infinitList" bindingFinder coreBindings
    describe "Custom Types" $ do
        it "function on custom type works" $ \(bindingFinder, coreBindings) -> do
            expectationForExpression "functionOnCustomType" bindingFinder coreBindings
        it "equality on custom type works" $ \(bindingFinder, coreBindings) -> do
            pendingWith "custom data types are not fully supported yet"
            --expectationForExpression "equalityOnCustomType" bindingFinder coreBindings

expectationForExpression :: String -> (String -> Expr Var) -> [Binding] -> Expectation
expectationForExpression expressionBindingName bindingFinder coreBindings = do
        let input = (bindingFinder (expressionBindingName ++ "Input"))
        let expectedOutput = (bindingFinder(expressionBindingName ++ "ExpectedOutput"))
        let output = reduceToHeadNormalForm coreBindings input
        (prepareExpressionArgumentForEvaluation output) `shouldBe` (prepareExpressionArgumentForEvaluation expectedOutput)

