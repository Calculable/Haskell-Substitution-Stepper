{-|
Module      : CorePrettyPrinter
Description : Printing Core expressions
License     : GPL-3

-}
module OriginalCoreAST.CorePrettyPrinter (prettyPrint) where

-- toDo: Typen entfernen, Cases vereinfachen, Nested, inline operator, alternativen umsortieren

import GHC.Plugins
import Utils
import Data.List
import OriginalCoreAST.CoreInformationExtractorFunctions
import OriginalCoreAST.CoreTypeClassInstances
import OriginalCoreAST.CoreStepperHelpers.CoreTransformer
import OriginalCoreAST.CoreTypeDefinitions
import GHC.Core.DataCon
type NestingLevel = Integer

-- |pretty-prints a Core Expression
prettyPrint :: CoreExpr -> IO ()
prettyPrint exp = prettyPrintLikeHaskell exp --alternative prettyPrint exp = prettyPrintOriginalHaskellCore exp

-- |pretty-prints a Core Expression using GHC's built-in pretty printer
prettyPrintOriginalHaskellCore :: CoreExpr -> IO ()
prettyPrintOriginalHaskellCore exp = putStr (prettyPrintToString exp)
  where
    prettyPrintToString :: CoreExpr -> String
    prettyPrintToString = showOutputable

-- |pretty-prints a Core Expression in a way that is easier to read and resemmbles the original Haskell syntax
prettyPrintLikeHaskell:: CoreExpr -> IO()
prettyPrintLikeHaskell expr = 
  putStrLn (showLikeHaskellWithIndentation 0 expr)
  where

    showLikeHaskellWithIndentation :: NestingLevel -> CoreExpr -> String
    showLikeHaskellWithIndentation nestingLevel expr = do
      let isOutermostLevel = nestingLevel == 0
      let expressionString = showLikeHaskellWithoutIndentation isOutermostLevel expr
      increaseIndentation nestingLevel expressionString
      where

        isMultiLineOrLongString :: String -> Bool
        isMultiLineOrLongString text = ('\n' `elem` text) || (length text > 85)

        increaseIndentation :: Integer -> String -> String
        increaseIndentation 0 text = text
        increaseIndentation x text = increaseIndentation (x-1) indentedText
          where indentedText = "  " ++ replace text "\n" "\n  "

        showLikeHaskellWithoutIndentation :: Bool -> CoreExpr -> String
        showLikeHaskellWithoutIndentation _ (Var var) = showVar var
        showLikeHaskellWithoutIndentation _ (Lit literal) = show literal
        showLikeHaskellWithoutIndentation _ (App expr arg) | isList (App expr arg) = showCoreList (getIndividualElementsOfList (App expr arg))
        showLikeHaskellWithoutIndentation _ (App expr arg) | isTuple (App expr arg) = showTuple (getIndividualElementsOfTuple (App expr arg))
        showLikeHaskellWithoutIndentation isOutermostLevel (App expr arg) = showApplication isOutermostLevel (convertToMultiArgumentFunction (App expr arg))
        showLikeHaskellWithoutIndentation isOutermostLevel (Lam bind expr) = showLam isOutermostLevel (convertToMultiArgumentLamda (Lam bind expr))
        showLikeHaskellWithoutIndentation _ (Let bind expr) = showLet (convertToMultiLet (Let bind expr))
        showLikeHaskellWithoutIndentation _ (Case expression binding caseType alternatives) = showCase expression binding caseType alternatives
        showLikeHaskellWithoutIndentation _ (Cast expression _) = showCast expression 
        showLikeHaskellWithoutIndentation _ (Tick _ expression ) = showTick expression 
        showLikeHaskellWithoutIndentation _ (Type ty) = showType ty 
        showLikeHaskellWithoutIndentation _ (Coercion _) = showCohercion

        showVar :: Var -> String
        showVar var = showOutputable (Var var :: CoreExpr)

        showCase :: CoreExpr -> Var -> Type -> [Alt Var] -> String
        showCase expr binding caseType alternatives = do
          let expressionString = showLikeHaskellWithoutIndentation False expr
          let alternativesString = intercalate ";\n" (map showAlternative alternatives)
          if isMultiLineOrLongString expressionString
            then  "case\n" ++ increaseIndentation 1 expressionString ++ "\nof {\n" ++ increaseIndentation 1 alternativesString ++ "\n}" 
            else "case " ++ expressionString ++ " of {\n" ++ increaseIndentation 1 alternativesString ++ "\n}"
          where

            showAlternative :: Alt Var -> String
            showAlternative (con, bindings, expr) = do
              let constructorString = showAlternativConstructor con
              let letBindingsString = unwords (map showVar bindings)
              let expressionString = showLikeHaskellWithoutIndentation False expr
              if isMultiLineOrLongString expressionString 
                then constructorString ++ " " ++ letBindingsString ++ " ->\n" ++ increaseIndentation 1 expressionString
                else constructorString ++ " " ++ letBindingsString ++ " -> " ++ expressionString
              where

                showAlternativConstructor :: AltCon -> String
                showAlternativConstructor (LitAlt lit) = showLikeHaskellWithoutIndentation False (Lit lit)
                showAlternativConstructor (DataAlt dataCon) = showOutputable dataCon
                showAlternativConstructor DEFAULT = "otherwise"


        showLet :: ([(Var, CoreExpr)], CoreExpr) -> String
        showLet (bindings, expr)  = do
          let expressionString = showLikeHaskellWithoutIndentation False expr
          let bindingExpressionStrings = map showBinding bindings
          expressionString ++ "\n  where\n" ++ increaseIndentation 2 (intercalate "\n" bindingExpressionStrings)
          where

            showBinding :: (Var, CoreExpr) -> String
            showBinding (var, expr) = do
              let expressionString = showLikeHaskellWithoutIndentation False expr
              if isMultiLineOrLongString expressionString
                then showVar var ++ " =\n" ++ increaseIndentation 1 expressionString
                else showVar var ++ " = " ++ expressionString

        showLam :: Bool -> ([Var], CoreExpr) -> String
        showLam isOutermostLevel (bindings, expr) = do
          let expressionString = showLikeHaskellWithoutIndentation False expr
          let parameterStrings = map showVar bindings
          let parameterListString = unwords parameterStrings
          let result = if isMultiLineOrLongString expressionString
                        then "\\" ++  parameterListString ++ " ->\n" ++ increaseIndentation 1 expressionString
                        else "\\" ++  parameterListString ++ " -> " ++ expressionString
          if isOutermostLevel
            then result
            else "(" ++ result ++ ")"

        showType :: Type -> String
        showType ty = "@" ++ showOutputable ty

        showCohercion :: String
        showCohercion = "(Coercion: not supported)"

        showTick :: CoreExpr -> String
        showTick expr = showLikeHaskellWithoutIndentation False expr

        showCast :: CoreExpr -> String
        showCast expr = showLikeHaskellWithoutIndentation False expr

        showApplication :: Bool -> (Function, [Argument]) -> String
        showApplication isOutermostLevel (function, arguments) = do
          let functionExpression = showLikeHaskellWithoutIndentation False function
          let argumentsExpression = map (showLikeHaskellWithoutIndentation False) arguments
          let result = if any isMultiLineOrLongString (functionExpression:argumentsExpression) 
                          then showMultiLineAppliation functionExpression  argumentsExpression
                          else showOneLineApplication functionExpression  argumentsExpression
          if isOutermostLevel
            then result
            else "(" ++ result ++ ")" 
          where

            showMultiLineAppliation :: String -> [String] -> String
            showMultiLineAppliation functionExpression argumentsExpression = functionExpression ++ "\n" ++ increaseIndentation 1 (intercalate "\n" argumentsExpression)

            showOneLineApplication :: String -> [String] -> String
            showOneLineApplication functionExpression argumentsExpression = unwords (functionExpression:argumentsExpression)


        showTuple :: [CoreExpr] -> String
        showTuple letListArguments = do
          let argumentsExpression = map (showLikeHaskellWithoutIndentation False) letListArguments
          if any isMultiLineOrLongString argumentsExpression 
            then showMultiLineTuple argumentsExpression
            else showOneLineTuple argumentsExpression
          where

            showOneLineTuple :: [String] -> String 
            showOneLineTuple arguments =  "(" ++ intercalate ", " arguments ++ ")"

            showMultiLineTuple :: [String] -> String 
            showMultiLineTuple arguments = do
              let separatedArguments = intercalate ",\n" arguments
              "(\n" ++ increaseIndentation 1 separatedArguments ++ ")"

        showCoreList :: [CoreExpr] -> String
        showCoreList letListArguments = do
          let argumentsExpression = map (showLikeHaskellWithoutIndentation False) letListArguments
          if any isMultiLineOrLongString argumentsExpression 
            then showMultiLineList argumentsExpression
            else showOneLineList argumentsExpression
          where
          
            showOneLineList :: [String] -> String 
            showOneLineList arguments =  "[" ++ intercalate ", " arguments ++ "]"

            showMultiLineList :: [String] -> String 
            showMultiLineList arguments = do
              let separatedArguments = intercalate ",\n" arguments
              "[\n" ++ increaseIndentation 1 separatedArguments ++ "]"

