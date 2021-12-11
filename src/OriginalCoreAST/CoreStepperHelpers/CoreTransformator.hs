module OriginalCoreAST.CoreStepperHelpers.CoreTransformator(convertFunctionApplicationWithArgumentListToNestedFunctionApplication, deepReplaceVarWithinExpression, deepReplaceVarWithinAlternative, deepReplaceMultipleVarWithinExpression, convertToMultiArgumentFunction, getIndividualElementsOfList)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.Maybe ()
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced, isEmptyList, isList, typeOfExpression)
import Debug.Trace(trace)
import Utils (showOutputable)

convertFunctionApplicationWithArgumentListToNestedFunctionApplication :: Expr Var -> [Expr Var] -> Expr Var
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression [] = expression
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression arguments = App (convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression (init arguments)) (last arguments)

deepReplaceVarWithinExpression :: Var -> Expr Var -> Expr Var -> Expr Var
deepReplaceVarWithinExpression name replaceExpression (Var varName) = if (==) (varToString varName) (varToString name) then replaceExpression else (Var varName)
deepReplaceVarWithinExpression name replaceExpression (App expression argument) = App (deepReplaceVarWithinExpression name replaceExpression expression) (deepReplaceVarWithinExpression name replaceExpression argument)
deepReplaceVarWithinExpression name replaceExpression (Lam parameter expression) =
  if (varToString parameter) == (varToString name)
    then Lam parameter expression --do nothing, use local lamda parameter with the same name (shadowing)
    else Lam parameter (deepReplaceVarWithinExpression name replaceExpression expression)
deepReplaceVarWithinExpression name replaceExpression (Case expression binding caseType alternatives) = Case (deepReplaceVarWithinExpression name replaceExpression expression) binding caseType (map (deepReplaceVarWithinAlternative name replaceExpression) alternatives)
deepReplaceVarWithinExpression name replaceExpression (Let binding expression) = Let (deepReplaceVarWithinBinding name replaceExpression binding) (deepReplaceVarWithinExpression name replaceExpression expression)
deepReplaceVarWithinExpression name replaceExpression (Cast expression cohersion) = Cast (deepReplaceVarWithinExpression name replaceExpression expression) cohersion
deepReplaceVarWithinExpression name replaceExpression (Type ty) = if (((==) (showOutputable ty) (showOutputable name)) && (isTypeInformation replaceExpression)) then replaceExpression else (Type ty)
deepReplaceVarWithinExpression _ _ expression = expression --nothing to replace (Coercion not implemented yet)

deepReplaceVarWithinAlternative :: Var -> Expr Var -> Alt Var -> Alt Var
deepReplaceVarWithinAlternative name replaceExpression (altCon, localBoundVars, expression) = if elem (varToString name) (map varToString localBoundVars)
                                                                                                 then (altCon, localBoundVars, expression) --do nothing, use local parameter with the same name (shadowing)
                                                                                                 else (altCon, localBoundVars, deepReplaceVarWithinExpression name replaceExpression expression)

deepReplaceVarWithinBinding :: Var -> Expr Var -> Bind Var -> Bind Var
deepReplaceVarWithinBinding name replaceExpression (NonRec b expression) = NonRec b newExpression
                                                                              where newExpression = snd (deepReplaceVarWithinBindingTuple name replaceExpression (b, expression))
deepReplaceVarWithinBinding name replaceExpression (Rec bindings) = Rec (map (deepReplaceVarWithinBindingTuple name replaceExpression) bindings)

deepReplaceVarWithinBindingTuple :: Var -> Expr Var -> (Var, Expr Var) -> (Var, Expr Var)
deepReplaceVarWithinBindingTuple name replaceExpression (b, expression) = if (==) (varToString b) (varToString name)
                                                                                then (b, expression) --do nothing (shadowing)
                                                                                else (b, deepReplaceVarWithinExpression name replaceExpression expression)


deepReplaceMultipleVarWithinExpression :: [Var] -> [Expr Var] -> Expr Var -> Expr Var
deepReplaceMultipleVarWithinExpression [] _ expression = expression
deepReplaceMultipleVarWithinExpression _ [] expression = expression
deepReplaceMultipleVarWithinExpression (x:xs) (y:ys) expression = deepReplaceMultipleVarWithinExpression xs ys (deepReplaceVarWithinExpression x y expression)

convertToMultiArgumentFunction :: Expr Var -> (Expr Var, [Expr Var])
convertToMultiArgumentFunction = collectArgs


getIndividualElementsOfList :: Expr Var -> [Expr Var]
getIndividualElementsOfList expr
  | isEmptyList expr = []
  | isList expr = do
      let (constructor, elements) = convertToMultiArgumentFunction expr
      if ((length elements) /= 3)
        then error ("unexpected number of arguments to cons operator: " ++ (show (length elements)))
        else do
          let [ty, first, nestedList] = take 3 elements
          [ty, first] ++ getIndividualElementsOfList nestedList
  | otherwise = []