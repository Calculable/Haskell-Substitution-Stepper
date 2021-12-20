{-|
Module      : CorePrettyPrinter
Description : Printing Core expressions
License     : GPL-3

-}
module OriginalCoreAST.CorePrettyPrinter (prettyPrint) where

import GHC.Plugins
import Utils
import Data.List
import OriginalCoreAST.CoreInformationExtractorFunctions
import OriginalCoreAST.CoreTypeClassInstances
import OriginalCoreAST.CoreStepperHelpers.CoreTransformer
import OriginalCoreAST.CoreTypeDefinitions
import GHC.Core.DataCon
import OriginalCoreAST.CoreStepperHelpers.CoreTracerHelper

-- |pretty-prints a Core Expression
prettyPrint :: CoreExpr -> IO ()
prettyPrint exp = prettyPrintLikeHaskell False exp --alternative: prettyPrint exp = prettyPrintOriginalHaskellCore exp

-- |pretty-prints a Core Expression using GHC's built-in pretty printer
prettyPrintOriginalHaskellCore :: CoreExpr -> IO ()
prettyPrintOriginalHaskellCore exp = putStr (prettyPrintToString exp)
  where
    prettyPrintToString :: CoreExpr -> String
    prettyPrintToString = showOutputable

-- |pretty-prints a Core Expression in a way that is easier to read and resemmbles the original Haskell syntax
prettyPrintLikeHaskell:: Bool -> CoreExpr -> IO()
prettyPrintLikeHaskell showTypes expr = 
  putStrLn (showLikeHaskellWithIndentation showTypes expr)
  where

    showLikeHaskellWithIndentation :: Bool -> CoreExpr -> String
    showLikeHaskellWithIndentation showTypes expr = do
      showLikeHaskell showTypes True expr
      where

        showLikeHaskell :: Bool -> Bool -> CoreExpr -> String
        showLikeHaskell _ _ (Var var) = showVar var
        showLikeHaskell _ _ (Lit literal) = show literal
        showLikeHaskell showTypes _ (App expr arg) | isList (App expr arg) = showCoreList showTypes (getIndividualElementsOfList (App expr arg))
        showLikeHaskell showTypes _ (App expr arg) | isTuple (App expr arg) = showTuple showTypes (getIndividualElementsOfTuple (App expr arg))
        showLikeHaskell showTypes isOutermostLevel (App expr arg) = do
          if isPrimitiveTypeConstructorApp (App expr arg)
            then showLikeHaskell showTypes isOutermostLevel arg 
            else showApplication showTypes isOutermostLevel (convertToMultiArgumentFunction (App expr arg))
        showLikeHaskell showTypes isOutermostLevel (Lam bind expr) = showLam showTypes isOutermostLevel (convertToMultiArgumentLamda (Lam bind expr))
        showLikeHaskell showTypes _ (Let bind expr) = showLet showTypes (convertToMultiLet (Let bind expr))
        showLikeHaskell showTypes _ (Case expression binding caseType alternatives) = showCase showTypes expression binding caseType alternatives
        showLikeHaskell showTypes isOutermostLevel (Cast expression _) = showCast showTypes isOutermostLevel expression 
        showLikeHaskell showTypes isOutermostLevel (Tick _ expression ) = showTick showTypes isOutermostLevel expression 
        showLikeHaskell _ _ (Type ty) = showType ty 
        showLikeHaskell _ _ (Coercion _) = showCohercion

        -- |checks if a printed expression expands over multiple lines or one single long line. 
        -- this check is used to pretty print expressions and add additional line breaks where useful
        isMultiLineOrLongExpression :: CoreExpr -> Bool
        isMultiLineOrLongExpression expression = do
          let expressionString = showLikeHaskell showTypes False expression
          isMultiLineOrLongString expressionString

          where
            isMultiLineOrLongString :: String -> Bool
            isMultiLineOrLongString text = ('\n' `elem` text) || (length text > 40)

        showVar :: Var -> String
        showVar var = do
          if isBoolVar (Var var) --find better solution for boolean workaround
            then varToSimpleString var
            else showOutputable (Var var :: CoreExpr)

        showCase :: Bool -> CoreExpr -> Var -> Type -> [Alt Var] -> String
        showCase showTypes expr binding caseType alternatives = do
          let resortedAlternatives = resortAlternatives [] alternatives --default case has to be the last not the first
          let expressionString = showLikeHaskell showTypes True expr
          let alternativesString = intercalate ";\n" (map showAlternative resortedAlternatives)
          if isMultiLineOrLongExpression expr
            then  "case\n" ++ increaseIndentation 1 expressionString ++ "\nof {\n" ++ increaseIndentation 1 alternativesString ++ "\n}" 
            else "case " ++ expressionString ++ " of {\n" ++ increaseIndentation 1 alternativesString ++ "\n}"
          where

            resortAlternatives :: [Alt Var] -> [Alt Var] -> [Alt Var]
            resortAlternatives previous ((altCon, bindings, expression) :rest) = 
              case altCon of {
                DEFAULT -> (previous ++ rest) ++ [(altCon, bindings, expression)]; --put default case last
                _ -> resortAlternatives (previous ++ [(altCon, bindings, expression)]) rest --search for default case
              }
            resortAlternatives previous [] = previous --there is not default case


            showAlternative :: Alt Var -> String
            showAlternative (con, bindings, expr) = do
              let constructorString = showAlternativConstructor con
              let letBindingsString = if null bindings 
                                        then ""
                                        else unwords (map showVar bindings) ++ " "
              let expressionString = showLikeHaskell showTypes False expr
              if isMultiLineOrLongExpression expr
                then constructorString ++ " " ++ letBindingsString ++ "->\n" ++ increaseIndentation 1 expressionString
                else constructorString ++ " " ++ letBindingsString ++ "-> " ++ expressionString
              where

                showAlternativConstructor :: AltCon -> String
                showAlternativConstructor (LitAlt lit) = showLikeHaskell showTypes False (Lit lit)
                showAlternativConstructor (DataAlt dataCon) = showOutputable dataCon
                showAlternativConstructor DEFAULT = "_"


        showLet :: Bool -> ([(Var, CoreExpr)], CoreExpr) -> String
        showLet showTypes (bindings, expr)  = do
          let expressionString = showLikeHaskell showTypes True expr
          let bindingExpressionStrings = map showBinding bindings
          expressionString ++ "\n  where\n" ++ increaseIndentation 2 (intercalate "\n" bindingExpressionStrings)
          where

            showBinding :: (Var, CoreExpr) -> String
            showBinding (var, expr) = do
              let expressionString = showLikeHaskell showTypes False expr
              if isMultiLineOrLongExpression expr
                then showVar var ++ " =\n" ++ increaseIndentation 1 expressionString
                else showVar var ++ " = " ++ expressionString

        showLam :: Bool -> Bool -> ([Var], CoreExpr) -> String
        showLam showTypes isOutermostLevel (bindings, expr) = do
          let expressionString = showLikeHaskell showTypes True expr
          let cleanUpBindings = optionallyRemoveTypeVars showTypes bindings
          let parameterStrings = map showVar cleanUpBindings
          let parameterListString = unwords parameterStrings
          let result = if isMultiLineOrLongExpression expr
                        then "\\" ++  parameterListString ++ " ->\n" ++ increaseIndentation 1 expressionString
                        else "\\" ++  parameterListString ++ " -> " ++ expressionString
          if isOutermostLevel
            then result
            else "(" ++ result ++ ")"

        showType :: Type -> String
        showType ty = "@" ++ showOutputable ty

        showCohercion :: String
        showCohercion = "(Coercion: not supported)"

        showTick :: Bool -> Bool -> CoreExpr -> String
        showTick showTypes isOutermostLevel expr = showLikeHaskell showTypes isOutermostLevel expr

        showCast :: Bool -> Bool -> CoreExpr -> String
        showCast showTypes isOutermostLevel expr = showLikeHaskell showTypes isOutermostLevel expr

        showApplication :: Bool -> Bool -> (Function, [Argument]) -> String
        showApplication showTypes isOutermostLevel (function, arguments) = do
          let cleanUpArguments = optionallyRemoveTypeInformation showTypes arguments
          let makeInlineFunctionApplication = isOperator function && (length cleanUpArguments == 2)
          let makeMultiLineApplication = any isMultiLineOrLongExpression (function:arguments) 
          let functionExpression = if makeInlineFunctionApplication && not makeMultiLineApplication
                                                          then showOperatorWithoutBrackets function
                                                          else showLikeHaskell showTypes False function

          let result = if makeMultiLineApplication
                          then showMultiLineAppliation functionExpression (map (showLikeHaskell showTypes True) cleanUpArguments)
                          else showOneLineApplication makeInlineFunctionApplication functionExpression (map (showLikeHaskell showTypes False) cleanUpArguments)
          if isOutermostLevel
            then result
            else "(" ++ result ++ ")" 
          where

            showMultiLineAppliation :: String -> [String] -> String
            showMultiLineAppliation functionExpression argumentsExpression = functionExpression ++ "\n" ++ increaseIndentation 1 (intercalate "\n" argumentsExpression)
            
            showOneLineApplication :: Bool -> String -> [String] -> String
            showOneLineApplication False functionExpression argumentsExpression = unwords (functionExpression:argumentsExpression)
            showOneLineApplication True functionExpression [firstArgument, secondArgument] = firstArgument ++ " " ++ functionExpression ++ " " ++ secondArgument
            showOneLineApplication True _ _ = error "inline function application needs exactly two arguments"

        showTuple :: Bool -> [CoreExpr] -> String
        showTuple showTypes tupleArguments = do
          let cleanUpArguments = optionallyRemoveTypeInformation showTypes tupleArguments
          let argumentsExpression = map (showLikeHaskell showTypes True) cleanUpArguments
          if any isMultiLineOrLongExpression tupleArguments 
            then showMultiLineTuple argumentsExpression
            else showOneLineTuple argumentsExpression
          where

            showOneLineTuple :: [String] -> String 
            showOneLineTuple arguments =  "(" ++ intercalate ", " arguments ++ ")"

            showMultiLineTuple :: [String] -> String 
            showMultiLineTuple arguments = do
              let separatedArguments = intercalate ",\n" arguments
              "(\n" ++ increaseIndentation 1 separatedArguments ++ ")"

        showCoreList :: Bool -> [CoreExpr] -> String
        showCoreList showTypes listArguments = do
          let cleanUpArguments = optionallyRemoveTypeInformation showTypes listArguments
          let argumentsExpression = map (showLikeHaskell showTypes True) cleanUpArguments
          if any isMultiLineOrLongExpression listArguments 
            then showMultiLineList argumentsExpression
            else showOneLineList argumentsExpression
          where
          
            showOneLineList :: [String] -> String 
            showOneLineList arguments =  "[" ++ intercalate ", " arguments ++ "]"

            showMultiLineList :: [String] -> String 
            showMultiLineList arguments = do
              let separatedArguments = intercalate ",\n" arguments
              "[\n" ++ increaseIndentation 1 separatedArguments ++ "\n]"

-- |takes a list of expressions and removes all type information if the first parameter is true
optionallyRemoveTypeInformation :: Bool -> [CoreExpr] -> [CoreExpr]
optionallyRemoveTypeInformation False expressions = removeTypeInformation expressions
optionallyRemoveTypeInformation True expressions = expressions

-- |takes a list of vars and removes all type information if the first parameter is true
optionallyRemoveTypeVars :: Bool -> [Var] -> [Var]
optionallyRemoveTypeVars False vars = removeTypeVars vars
optionallyRemoveTypeVars True vars = vars

