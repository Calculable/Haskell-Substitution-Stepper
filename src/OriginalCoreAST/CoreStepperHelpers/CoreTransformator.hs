module OriginalCoreAST.CoreStepperHelpers.CoreTransformator (convertFunctionApplicationWithArgumentListToNestedFunctionApplication, deepReplaceVarWithinExpression, deepReplaceVarWithinAlternative, deepReplaceMultipleVarWithinExpression, convertToMultiArgumentFunction) where

import GHC.Plugins
  ( Alt,
    Bind (..),
    Expr (App, Case, Lam, Let, Var),
    Var,
    collectArgs,
  )
import OriginalCoreAST.CoreInformationExtractorFunctions
  ( varToString,
  )
import OriginalCoreAST.CoreMakerFunctions ()
import OriginalCoreAST.CoreTypeClassInstances ()

convertFunctionApplicationWithArgumentListToNestedFunctionApplication :: Expr Var -> [Expr Var] -> Expr Var
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression [] = expression
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression arguments = App (convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression (init arguments)) (last arguments)

deepReplaceVarWithinExpression :: Var -> Expr Var -> Expr Var -> Expr Var
deepReplaceVarWithinExpression name replaceExpression (Var varName) = if (==) (varToString varName) (varToString name) then replaceExpression else Var varName
deepReplaceVarWithinExpression name replaceExpression (App expression argument) = App (deepReplaceVarWithinExpression name replaceExpression expression) (deepReplaceVarWithinExpression name replaceExpression argument)
deepReplaceVarWithinExpression name replaceExpression (Lam parameter expression) =
  if varToString parameter == varToString name
    then Lam parameter expression --do nothing, use local lamda parameter with the same name (shadowing)
    else Lam parameter (deepReplaceVarWithinExpression name replaceExpression expression)
deepReplaceVarWithinExpression name replaceExpression (Case expression binding caseType alternatives) = Case (deepReplaceVarWithinExpression name replaceExpression expression) binding caseType (map (deepReplaceVarWithinAlternative name replaceExpression) alternatives)
deepReplaceVarWithinExpression name replaceExpression (Let binding expression) = Let (deepReplaceVarWithinBinding name replaceExpression binding) (deepReplaceVarWithinExpression name replaceExpression expression)
deepReplaceVarWithinExpression _ _ expression = expression --nothing to replace (ToDo: Cast and Coercion not implemented yet)

deepReplaceVarWithinAlternative :: Var -> Expr Var -> Alt Var -> Alt Var
deepReplaceVarWithinAlternative name replaceExpression (altCon, localBoundVars, expression) =
  if varToString name `elem` map varToString localBoundVars
    then (altCon, localBoundVars, expression) --do nothing, use local parameter with the same name (shadowing)
    else (altCon, localBoundVars, deepReplaceVarWithinExpression name replaceExpression expression)

deepReplaceVarWithinBinding :: Var -> Expr Var -> Bind Var -> Bind Var
deepReplaceVarWithinBinding name replaceExpression (NonRec b expression) = NonRec b newExpression
  where
    newExpression = snd (deepReplaceVarWithinBindingTuple name replaceExpression (b, expression))
deepReplaceVarWithinBinding name replaceExpression (Rec bindings) = Rec (map (deepReplaceVarWithinBindingTuple name replaceExpression) bindings)

deepReplaceVarWithinBindingTuple :: Var -> Expr Var -> (Var, Expr Var) -> (Var, Expr Var)
deepReplaceVarWithinBindingTuple name replaceExpression (b, expression) =
  if (==) (varToString b) (varToString name)
    then (b, expression) --do nothing (shadowing)
    else (b, deepReplaceVarWithinExpression name replaceExpression expression)

deepReplaceMultipleVarWithinExpression :: [Var] -> [Expr Var] -> Expr Var -> Expr Var
deepReplaceMultipleVarWithinExpression [] _ expression = expression
deepReplaceMultipleVarWithinExpression _ [] expression = expression
deepReplaceMultipleVarWithinExpression (x : xs) (y : ys) expression = deepReplaceMultipleVarWithinExpression xs ys (deepReplaceVarWithinExpression x y expression)

convertToMultiArgumentFunction :: Expr Var -> (Expr Var, [Expr Var])
convertToMultiArgumentFunction = collectArgs
