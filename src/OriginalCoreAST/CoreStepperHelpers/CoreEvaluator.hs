module OriginalCoreAST.CoreStepperHelpers.CoreEvaluator(evaluateFunctionWithArguments)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.Maybe ()
import GHC.Core (Expr (..))
import GHC.Types.Literal ( Literal (..))
import GHC.Types.Var (Var)
import Debug.Trace(trace)
import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced)

evaluateFunctionWithArguments :: Expr Var -> [Expr Var] -> Maybe (Expr Var)
evaluateFunctionWithArguments (Var functionOrOperatorName) arguments = do
    let argumentsWithoutApplications = map replaceApplicationWithArgument arguments --sometimes, arguments are of type "application". For example for a Double: "D# 0.5##". ToDo: Check if this command is dangerous
    evaluateUnsteppableFunctionWithArguments (varToString functionOrOperatorName) (filter (not.isTypeInformation) argumentsWithoutApplications) --Precondition: function must be in the form of "var". This is already checked by the function which is calling this function.
evaluateFunctionWithArguments _ _ = error "function-expression has to be a 'Var'"

replaceApplicationWithArgument :: Expr Var -> Expr Var
replaceApplicationWithArgument (App expr arg) = arg
replaceApplicationWithArgument x = x



evaluateUnsteppableFunctionWithArguments :: String -> [Expr Var] -> Maybe (Expr Var)
evaluateUnsteppableFunctionWithArguments "+" [x, y] = Just ((+) x y)
evaluateUnsteppableFunctionWithArguments "-" [x, y] = Just ((-) x y)
evaluateUnsteppableFunctionWithArguments "*" [x, y] = Just ((*) x y)
evaluateUnsteppableFunctionWithArguments "/" [x, y] = Just ((/) x y)
evaluateUnsteppableFunctionWithArguments "recip" [x] = Just (recip x)
evaluateUnsteppableFunctionWithArguments "signum" [x] = Just (signum x)
evaluateUnsteppableFunctionWithArguments "abs" [x] = Just (abs x)
evaluateUnsteppableFunctionWithArguments "negate" [x] = Just (negate x)
evaluateUnsteppableFunctionWithArguments "fromInteger" [(Lit (LitNumber _ x))] = Just (fromInteger x)
evaluateUnsteppableFunctionWithArguments "/=" [x, y] = Just (boolToCoreExpression ((/=) x y))
evaluateUnsteppableFunctionWithArguments "==" [x, y] = Just (boolToCoreExpression ((==) x y))
evaluateUnsteppableFunctionWithArguments "<" [x, y] = Just (boolToCoreExpression ((<) x y))
evaluateUnsteppableFunctionWithArguments ">" [x, y] = Just (boolToCoreExpression ((>) x y))
evaluateUnsteppableFunctionWithArguments ">=" [x, y] = Just (boolToCoreExpression ((>=) x y))
evaluateUnsteppableFunctionWithArguments "<=" [x, y] = Just (boolToCoreExpression ((<=) x y))
evaluateUnsteppableFunctionWithArguments "min" [x, y] = Just $ min x y
evaluateUnsteppableFunctionWithArguments "max" [x, y] = Just $ max x y
evaluateUnsteppableFunctionWithArguments "unpackCString#" [x] = Just x
evaluateUnsteppableFunctionWithArguments "succ" [x] = Just $ succ x
evaluateUnsteppableFunctionWithArguments "pred" [x] = Just $ pred x
evaluateUnsteppableFunctionWithArguments "fromEnum" [x] = Just $ integerToCoreExpression  (toInteger (fromEnum x))
evaluateUnsteppableFunctionWithArguments "exp" [x] = Just (exp x)
evaluateUnsteppableFunctionWithArguments "log" [x] = Just (log x)
evaluateUnsteppableFunctionWithArguments "sqrt" [x] = Just (sqrt x)
evaluateUnsteppableFunctionWithArguments "sin" [x] = Just (sin x)
evaluateUnsteppableFunctionWithArguments "cos" [x] = Just (cos x)
evaluateUnsteppableFunctionWithArguments "tan" [x] = Just (tan x)
evaluateUnsteppableFunctionWithArguments "asin" [x] = Just (asin x)
evaluateUnsteppableFunctionWithArguments "acos" [x] = Just (acos x)
evaluateUnsteppableFunctionWithArguments "atan" [x] = Just (atan x)
evaluateUnsteppableFunctionWithArguments "sinh" [x] = Just (sinh x)
evaluateUnsteppableFunctionWithArguments "cosh" [x] = Just (cosh x)
evaluateUnsteppableFunctionWithArguments "tanh" [x] = Just (tanh x)
evaluateUnsteppableFunctionWithArguments "asinh" [x] = Just (asinh x)
evaluateUnsteppableFunctionWithArguments "acosh" [x] = Just (acosh x)
evaluateUnsteppableFunctionWithArguments "atanh" [x] = Just (atanh x)
evaluateUnsteppableFunctionWithArguments "**" [x, y] = Just ((**) x y)
evaluateUnsteppableFunctionWithArguments "logBase" [x, y] = Just (logBase x y)
evaluateUnsteppableFunctionWithArguments "quot" [x, y] = Just (quot x y)
evaluateUnsteppableFunctionWithArguments "rem" [x, y] = Just (rem x y)
evaluateUnsteppableFunctionWithArguments "div" [x, y] = Just (div x y)
evaluateUnsteppableFunctionWithArguments "mod" [x, y] = Just (mod x y)
evaluateUnsteppableFunctionWithArguments "toInteger" [x] = Just (integerToCoreExpression (toInteger x))
evaluateUnsteppableFunctionWithArguments "toRational" [x] = Just (rationalToCoreExpression (toRational x))
evaluateUnsteppableFunctionWithArguments "floatRadix" [x] = Just $ integerToCoreExpression (floatRadix x)
evaluateUnsteppableFunctionWithArguments "floatDigits" [x] = Just $ integerToCoreExpression (toInteger (floatDigits x))
evaluateUnsteppableFunctionWithArguments "encodeFloat" [(Lit (LitNumber _ x)), (Lit (LitNumber _ y))] = Just (encodeFloat x (fromIntegral y))
evaluateUnsteppableFunctionWithArguments "exponent" [x] = Just (integerToCoreExpression (toInteger (exponent x)))
evaluateUnsteppableFunctionWithArguments "significand" [x] = Just (significand x)
evaluateUnsteppableFunctionWithArguments "scaleFloat" [(Lit (LitNumber _ x)), y] = Just (scaleFloat (fromIntegral x) y)
evaluateUnsteppableFunctionWithArguments "isNaN" [x] = Just (boolToCoreExpression (isNaN x))
evaluateUnsteppableFunctionWithArguments "isInfinite" [x] = Just (boolToCoreExpression (isInfinite x))
evaluateUnsteppableFunctionWithArguments "isDenormalized" [x] = Just (boolToCoreExpression (isDenormalized x))
evaluateUnsteppableFunctionWithArguments "isNegativeZero" [x] = Just (boolToCoreExpression (isNegativeZero x))
evaluateUnsteppableFunctionWithArguments "isIEEE" [x] = Just (boolToCoreExpression (isIEEE x))
evaluateUnsteppableFunctionWithArguments "atan2" [x, y] = Just (atan2 x y)
evaluateUnsteppableFunctionWithArguments "truncate" [x] = Just (integerToCoreExpression (toInteger (truncate x)))
evaluateUnsteppableFunctionWithArguments "round" [x] = Just (integerToCoreExpression (toInteger (round x)))
evaluateUnsteppableFunctionWithArguments "ceiling" [x] = Just (integerToCoreExpression (toInteger (ceiling x)))
evaluateUnsteppableFunctionWithArguments "floor" [x] = Just (integerToCoreExpression (toInteger (floor x)))

evaluateUnsteppableFunctionWithArguments name _ = Nothing --function not supported
--toDo: Implement more operators and functions