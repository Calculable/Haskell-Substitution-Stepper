module OriginalCoreAST.CoreStepperHelpers.CoreEvaluator(applyFunctionToArguments)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.Maybe
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)

import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToExpression)
import OriginalCoreAST.CoreInformationExtractorFunctions(showVarExpression, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced)


applyFunctionToArguments :: Expr Var -> [Expr Var] -> Maybe (Expr Var) 
applyFunctionToArguments (Var functionOrOperatorName) arguments = do
    applyUnsteppableFunctionToArguments (varToString functionOrOperatorName) (filter (not.isTypeInformation) arguments) --Precondition: function must be in the form of "var" and all arguments must be in the form of. This is already checked by the function which is calling this function
applyFunctionToArguments _ _ = error "function-expression has to be a 'Var'"

applyUnsteppableFunctionToArguments :: String -> [Expr Var] -> Maybe (Expr Var) 
applyUnsteppableFunctionToArguments "+" [x, y] = Just ((+) x y)
applyUnsteppableFunctionToArguments "-" [x, y] = Just ((-) x y)
applyUnsteppableFunctionToArguments "*" [x, y] = Just ((*) x y)
applyUnsteppableFunctionToArguments "/" [x, y] = Just ((/) x y)
applyUnsteppableFunctionToArguments "recip" [x] = Just (recip x)
applyUnsteppableFunctionToArguments "signum" [x] = Just (signum x)
applyUnsteppableFunctionToArguments "abs" [x] = Just (abs x)
applyUnsteppableFunctionToArguments "/=" [x, y] = Just (boolToExpression ((/=) x y))
applyUnsteppableFunctionToArguments "==" [x, y] = Just (boolToExpression ((==) x y))
applyUnsteppableFunctionToArguments "<" [x, y] = Just (boolToExpression ((<) x y))
applyUnsteppableFunctionToArguments ">" [x, y] = Just (boolToExpression ((>) x y))
applyUnsteppableFunctionToArguments ">=" [x, y] = Just (boolToExpression ((>=) x y))
applyUnsteppableFunctionToArguments "<=" [x, y] = Just (boolToExpression ((<=) x y))
applyUnsteppableFunctionToArguments "negate" [(Lit (LitNumber _ x))] = Just (integerToCoreExpression (negate x)) --example of an arbitrary function from the prelude. note how the arguments must have the right type and the result is converted back into an expressino
applyUnsteppableFunctionToArguments "unpackCString#" [x] = Just x
applyUnsteppableFunctionToArguments name _ = Nothing --function not supported
