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
module CoreAST.IntegrationTestSpec where

import DataProvider.DataProvider
  ( getBindingFinderWithCoreBindings,
  )
import GHC.Plugins (Expr, Var)
import CoreAST.Stepper (reduceToNormalForm)
import CoreAST.Helpers.Transformer
  ( prepareExpressionArgumentForEvaluation,
  )
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
spec = beforeAll (getBindingFinderWithCoreBindings "test/TestBindings.hs") $ do
  describe "Arithmetic Operators" $ do
    it "can reduce addition" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "addition" bindingFinder coreBindings
    it "can reduce addition with application syntax" $ \(bindingFinder, coreBindings) -> do
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
      expectationForExpression "basicOperationOnBoolean" bindingFinder coreBindings
    it "can reduce basic operation on Char" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "basicOperationOnChar" bindingFinder coreBindings
    it "can reduce basic operation on Double" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "basicOperationOnDouble" bindingFinder coreBindings
    it "can reduc basic operation on Int" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "basicOperationOnInt" bindingFinder coreBindings
    it "can reduce basic operation on Integer" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "basicOperationOnInteger" bindingFinder coreBindings
    it "can reduce basic operation on Either" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "basicOperationOnEither" bindingFinder coreBindings
    it "can reduce basic operation on Maybe" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "basicOperationOnMaybe" bindingFinder coreBindings
    it "can reduce basic operation on  Rational" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "basicOperationOnRational" bindingFinder coreBindings
    it "can reduce basic operation on String" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "basicOperationOnString" bindingFinder coreBindings
    it "can reduce basic operation on List" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "basicOperationOnList" bindingFinder coreBindings
    it "can reduce basic operation on Tuple" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "basicOperationOnTuple" bindingFinder coreBindings
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
  describe "Support for Bounded Type" $ do
    it "maxBound works" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "maxBoundInt" bindingFinder coreBindings
    it "minBound works" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "minBoundBool" bindingFinder coreBindings
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
    it "local pattern matching works (variant 1)" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "localPatternMatchingVariant1" bindingFinder coreBindings
    it "local pattern matching works (variant 2)" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "localPatternMatchingVariant2" bindingFinder coreBindings
  describe "Recursion" $ do
    it "recursion works" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "recursion" bindingFinder coreBindings
  describe "List Operations" $ do
    it "list operations work" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "listOperations" bindingFinder coreBindings
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
      expectationForExpression "equalsOnTuple" bindingFinder coreBindings
  describe "Infinite List" $ do
    it "Infinite lists works as function parameter" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "infiniteList" bindingFinder coreBindings
  describe "Bounded Lists" $ do
    it "Bounded lists works as function parameter" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "boundedList" bindingFinder coreBindings
  describe "Custom Types" $ do
    it "function on custom type works" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "functionOnCustomType" bindingFinder coreBindings
    it "equality on custom type works" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "equalityOnCustomType" bindingFinder coreBindings
  describe "Map works" $ do
    it "map on lists works" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "map" bindingFinder coreBindings
  describe "fmap works" $ do
    it "fmap on Just-Maybe works" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "fmapOnJust" bindingFinder coreBindings
    it "fmap on Nothing-Maybe works" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "fmapOnNothing" bindingFinder coreBindings
    it "fmap on List works" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "fmapOnList" bindingFinder coreBindings
  describe "Monads are supported" $ do
    it "maybe monad works (success case)" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "monadMaybe" bindingFinder coreBindings
    it "list monad works (success case)" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "monadList" bindingFinder coreBindings
  describe "List generator works" $ do
    it "nested list generator works" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "generator" bindingFinder coreBindings
  describe "Functions that might throw an error are supported" $ do
    it "functions that might throw an error can be called" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "functionThatMightThrowError" bindingFinder coreBindings
  describe "Custom Type Classes" $ do
    it "standart type classes can be implemented" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "usageOfStandardTypeClass" bindingFinder coreBindings
    it "custom type classes can be implemented" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "usageOfCustomTypeClass" bindingFinder coreBindings
    it "automatic derived type classes work" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "usageOfAutomaticDerivedTypeClass" bindingFinder coreBindings
    it "custom type classes work" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "usageOfCustomTypeClass2" bindingFinder coreBindings
  describe "Support for the Standard Prelude from The Haskell 98 Report" $ do
    it "maybeType" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "maybeType" bindingFinder coreBindings
    it "fmap on maybe just" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "fmapOnMaybeJust" bindingFinder coreBindings
    it "fmap on nothing" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "fmapOnMaybeNothing" bindingFinder coreBindings
    it "maybe monad" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "maybeAsMonadComparison" bindingFinder coreBindings
    it "ord on maybe" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "ordOnMaybe" bindingFinder coreBindings
    it "equality on ordering" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "equalityOnOrdering" bindingFinder coreBindings
    it "either fromRight" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "fromRightFunction" bindingFinder coreBindings
    it "fmap on either" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "fMapOnEither" bindingFinder coreBindings
    it "isLeft on either" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "isLeftOnRight" bindingFinder coreBindings
    it "fst on tuple" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "firstElement" bindingFinder coreBindings
    it "snd on tuple" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "secondElement" bindingFinder coreBindings
    it "even" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "evenOnUnevenNumber" bindingFinder coreBindings
    it "power operator" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "powerOperator" bindingFinder coreBindings
    it "id" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "idFunction" bindingFinder coreBindings
    it "concatenated functions" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "concatenatedFunctions" bindingFinder coreBindings
    it "dollar operator" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "dollarOperator" bindingFinder coreBindings
    it "and operator" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "andOperator" bindingFinder coreBindings
    it "or operator" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "orOperator" bindingFinder coreBindings
    it "map on list" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "mapOnList" bindingFinder coreBindings
    it "filter on list" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "filterList" bindingFinder coreBindings
    it "concatenation of lists" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "concatList" bindingFinder coreBindings
    it "index operator" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "indexOperator" bindingFinder coreBindings
    it "foldl" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "foldlFunction" bindingFinder coreBindings
    it "iterate" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "iterateFunction" bindingFinder coreBindings
    it "zip" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "zipFunction" bindingFinder coreBindings
    it "enum from char" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "enumFromChar" bindingFinder coreBindings
    it "enum from to char" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "enumFromToChar" bindingFinder coreBindings
    it "enum from then to int" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "enumFromThenToInt" bindingFinder coreBindings
    it "succ on double" $ \(bindingFinder, coreBindings) -> do
      expectationForExpression "succOnDouble" bindingFinder coreBindings
  describe "Imports" $ do
    it "other modules can be imported" $ \(bindingFinder, coreBindings) -> do
      pendingWith "import is not yet supported"


expectationForExpression :: String -> (String -> Expr Var) -> [Binding] -> Expectation
expectationForExpression expressionBindingName bindingFinder coreBindings = do
  let input = bindingFinder (expressionBindingName ++ "Input")
  let expectedOutput = bindingFinder (expressionBindingName ++ "ExpectedOutput")
  let output = reduceToNormalForm coreBindings input
  prepareExpressionArgumentForEvaluation output `shouldBe` prepareExpressionArgumentForEvaluation expectedOutput
