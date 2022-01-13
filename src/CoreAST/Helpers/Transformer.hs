-- |
-- Module      : CoreTransformer
-- Description : Transform Core expressions
-- License     : GPL-3
--
-- This module contains helper functions to manipulate and transform Core expressions.
module CoreAST.Helpers.Transformer
  ( convertFunctionApplicationWithArgumentListToNestedFunctionApplication,
    deepReplaceVarWithinExpression,
    deepReplaceMultipleVarWithinExpression,
    convertToMultiArgumentFunction,
    prepareExpressionArgumentForEvaluation,
    extractArgumentsOfNestedApplication,
  )
where

import CoreAST.InformationExtractor
  ( isTypeInformation,
    isTypeWrapperFunctionName,
    varToString,
  )
import CoreAST.TypeDefinitions (Argument, Function)
import GHC.Plugins
  ( Alt,
    Bind (..),
    CoreExpr,
    Expr (App, Case, Cast, Lam, Let, Type, Var),
    Var,
    collectArgs,
  )
import Utils (showOutputable)

-- | Replace all occurances of a given var inside an expression with another expression
deepReplaceVarWithinExpression :: Var -> CoreExpr -> CoreExpr -> CoreExpr
deepReplaceVarWithinExpression name replaceExpression (Var varName) =
  if (==) (varToString varName) (varToString name)
    then replaceExpression
    else Var varName
deepReplaceVarWithinExpression name replaceExpression (App expression argument) =
  App (deepReplaceVarWithinExpression name replaceExpression expression) (deepReplaceVarWithinExpression name replaceExpression argument)
deepReplaceVarWithinExpression name replaceExpression (Lam parameter expression) =
  if varToString parameter == varToString name
    then Lam parameter expression --do nothing, use local lamda parameter with the same name (shadowing)
    else Lam parameter (deepReplaceVarWithinExpression name replaceExpression expression)
deepReplaceVarWithinExpression name replaceExpression (Case expression binding caseType alternatives) =
  Case (deepReplaceVarWithinExpression name replaceExpression expression) binding caseType (map (deepReplaceVarWithinAlternative name replaceExpression) alternatives)
deepReplaceVarWithinExpression name replaceExpression (Let binding expression) =
  Let (deepReplaceVarWithinBinding name replaceExpression binding) (deepReplaceVarWithinExpression name replaceExpression expression)
deepReplaceVarWithinExpression name replaceExpression (Cast expression cohersion) =
  Cast (deepReplaceVarWithinExpression name replaceExpression expression) cohersion
deepReplaceVarWithinExpression name replaceExpression (Type ty) =
  if (==) (showOutputable ty) (showOutputable name) && isTypeInformation replaceExpression
    then replaceExpression
    else Type ty
deepReplaceVarWithinExpression _ _ expression = expression --nothing to replace

-- | Replace all occurances of a given var inside an alternative with an expression
deepReplaceVarWithinAlternative :: Var -> CoreExpr -> Alt Var -> Alt Var
deepReplaceVarWithinAlternative name replaceExpression (altCon, localBoundVars, expression) =
  if varToString name `elem` map varToString localBoundVars
    then (altCon, localBoundVars, expression) --do nothing, use local parameter with the same name (shadowing)
    else (altCon, localBoundVars, deepReplaceVarWithinExpression name replaceExpression expression)

-- | Replace all occurances of a given var inside an binding with an expression
deepReplaceVarWithinBinding :: Var -> CoreExpr -> Bind Var -> Bind Var
deepReplaceVarWithinBinding name replaceExpression (NonRec b expression) = NonRec b newExpression
  where
    newExpression = snd (deepReplaceVarWithinBindingTuple name replaceExpression (b, expression))
deepReplaceVarWithinBinding name replaceExpression (Rec bindings) = Rec (map (deepReplaceVarWithinBindingTuple name replaceExpression) bindings)

deepReplaceVarWithinBindingTuple :: Var -> CoreExpr -> (Var, CoreExpr) -> (Var, CoreExpr)
deepReplaceVarWithinBindingTuple name replaceExpression (b, expression) =
  if (==) (varToString b) (varToString name)
    then (b, expression) --do nothing (shadowing)
    else (b, deepReplaceVarWithinExpression name replaceExpression expression)

-- | Replace all occurances of specific vars inside an expression with other expressions.
--  The caller has to provide two lists: The fist list contains all Var-occurances that
--  should be replaced with an expression from the second list. The Var at index 0
--  gets replaced with the expression at index 0 and so on
deepReplaceMultipleVarWithinExpression :: [Var] -> [CoreExpr] -> CoreExpr -> CoreExpr
deepReplaceMultipleVarWithinExpression [] _ expression = expression
deepReplaceMultipleVarWithinExpression _ [] expression = expression
deepReplaceMultipleVarWithinExpression (x : xs) (y : ys) expression = deepReplaceMultipleVarWithinExpression xs ys (deepReplaceVarWithinExpression x y expression)

-- | Unwrapps the argument from a type wrapper if necessary
prepareExpressionArgumentForEvaluation :: CoreExpr -> CoreExpr
prepareExpressionArgumentForEvaluation (App (Var id) arg) =
  if isTypeWrapperFunctionName (varToString id)
    then arg
    else App (Var id) arg
prepareExpressionArgumentForEvaluation x = x

-- | Extracts all the arguments from a nested application as a list
extractArgumentsOfNestedApplication :: CoreExpr -> [Argument]
extractArgumentsOfNestedApplication expr = snd (convertToMultiArgumentFunction expr)

-- | Extracts the function and a list of arguments from a nested application.
--  Haskell Core does not know the concept of multi-argument function and
--  uses partial function application instead. For example
--  an expression like "functionWithThreeArguments 1 2 3" is represented
--  in Core like: ((functionWithThreeArguments 1) 2) 3).
--  With the same example, the result of this function would be the following tuple:
--  (functionWithThreeArguments, [1, 2, 3])
convertToMultiArgumentFunction :: CoreExpr -> (Function, [Argument])
convertToMultiArgumentFunction = collectArgs

-- | Takes a function expression and a list of arguments and converts it into a Core-Like
--  nested function application. Haskell Core does not know the concept of multi-argument function and
--  uses partial function application instead. This function is the inverse-function of "convertToMultiArgumentFunction"
convertFunctionApplicationWithArgumentListToNestedFunctionApplication :: Function -> [Argument] -> Expr Var
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression [] = expression
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression arguments = App (convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression (init arguments)) (last arguments)
