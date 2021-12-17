module OriginalCoreAST.CoreStepperHelpers.CoreEvaluator (evaluateFunctionWithArguments) where
import GHC.Plugins hiding (($$))
import OriginalCoreAST.CoreInformationExtractorFunctions
import Data.Maybe
import OriginalCoreAST.CoreMakerFunctions
    ( boolToCoreExpression,
      charToCoreExpression,
      expressionTupleToCoreTuple,
      integerToCoreExpression,
      rationalToCoreExpression )
import Data.Bifunctor
import Control.Monad
import OriginalCoreAST.CoreStepperHelpers.CoreEvaluatorHelper
import OriginalCoreAST.CoreTypeClassInstances ()

import OriginalCoreAST.CoreStepperHelpers.CoreTransformator
import Data.Char
import Utils

type Binding = (Var, Expr Var)
type Reducer = (Expr Var -> Maybe (Expr Var))

evaluateFunctionWithArguments :: Expr Var -> [Expr Var] -> Reducer -> Maybe (Expr Var)
evaluateFunctionWithArguments (Var functionOrOperatorName) arguments reducer = 
    if isJust evaluationWithTypes
      then evaluationWithTypes
      else evaluationWithoutTypes
    where
      evaluationWithTypes = evaluateUnsteppableFunctionWithArgumentsAndTypes (varToString functionOrOperatorName) argumentsWithoutApplications reducer
      evaluationWithoutTypes = evaluateUnsteppableFunctionWithArguments (varToString functionOrOperatorName) (removeTypeInformation argumentsWithoutApplications) reducer
      argumentsWithoutApplications = map prepareExpressionArgumentForEvaluation arguments
evaluateFunctionWithArguments _ _ _ = error "function-expression has to be a 'Var'"


evaluateUnsteppableFunctionWithArguments :: String -> [Expr Var] -> Reducer -> Maybe (Expr Var)
evaluateUnsteppableFunctionWithArguments "+" [x, y] _ = Just ((+) x y)
evaluateUnsteppableFunctionWithArguments "-" [x, y] _ = Just ((-) x y)
evaluateUnsteppableFunctionWithArguments "*" [x, y] _ = Just ((*) x y)
evaluateUnsteppableFunctionWithArguments "/" [x, y] _ = Just ((/) x y)
evaluateUnsteppableFunctionWithArguments "recip" [x] _ = Just (recip x)
evaluateUnsteppableFunctionWithArguments "signum" [x] _ = Just (signum x)
evaluateUnsteppableFunctionWithArguments "abs" [x] _ = Just (abs x)
evaluateUnsteppableFunctionWithArguments "negate" [x] _ = Just (negate x)
evaluateUnsteppableFunctionWithArguments "fromInteger" [Lit (LitNumber _ x)] _ = Just (fromInteger x)
evaluateUnsteppableFunctionWithArguments "/=" [x, y] _ = Just (boolToCoreExpression ((/=) x y))
evaluateUnsteppableFunctionWithArguments "==" [x, y] reducer = Just (boolToCoreExpression ((==) (reducer x) (reducer y)))
evaluateUnsteppableFunctionWithArguments "<" [x, y] _ = Just (boolToCoreExpression ((<) x y))
evaluateUnsteppableFunctionWithArguments ">" [x, y] _ = Just (boolToCoreExpression ((>) x y))
evaluateUnsteppableFunctionWithArguments ">=" [x, y] _ = Just (boolToCoreExpression ((>=) x y))
evaluateUnsteppableFunctionWithArguments "<=" [x, y] _ = Just (boolToCoreExpression ((<=) x y))
evaluateUnsteppableFunctionWithArguments "min" [x, y] _ = Just $ min x y
evaluateUnsteppableFunctionWithArguments "max" [x, y] _ = Just $ max x y
evaluateUnsteppableFunctionWithArguments "succ" [x] _ = Just $ succ x
evaluateUnsteppableFunctionWithArguments "pred" [x] _ = Just $ pred x
evaluateUnsteppableFunctionWithArguments "fromEnum" [x] _ = Just $ integerToCoreExpression (toInteger (fromEnum x))
evaluateUnsteppableFunctionWithArguments "exp" [x] _ = Just (exp x)
evaluateUnsteppableFunctionWithArguments "log" [x] _ = Just (log x)
evaluateUnsteppableFunctionWithArguments "sqrt" [x] _ = Just (sqrt x)
evaluateUnsteppableFunctionWithArguments "sin" [x] _ = Just (sin x)
evaluateUnsteppableFunctionWithArguments "cos" [x] _ = Just (cos x)
evaluateUnsteppableFunctionWithArguments "tan" [x] _ = Just (tan x)
evaluateUnsteppableFunctionWithArguments "asin" [x] _ = Just (asin x)
evaluateUnsteppableFunctionWithArguments "acos" [x] _ = Just (acos x)
evaluateUnsteppableFunctionWithArguments "atan" [x] _ = Just (atan x)
evaluateUnsteppableFunctionWithArguments "sinh" [x] _ = Just (sinh x)
evaluateUnsteppableFunctionWithArguments "cosh" [x] _ = Just (cosh x)
evaluateUnsteppableFunctionWithArguments "tanh" [x] _ = Just (tanh x)
evaluateUnsteppableFunctionWithArguments "asinh" [x] _ = Just (asinh x)
evaluateUnsteppableFunctionWithArguments "acosh" [x] _ = Just (acosh x)
evaluateUnsteppableFunctionWithArguments "atanh" [x] _ = Just (atanh x)
evaluateUnsteppableFunctionWithArguments "**" [x, y] _ = Just ((**) x y)
evaluateUnsteppableFunctionWithArguments "logBase" [x, y] _ = Just (logBase x y)
evaluateUnsteppableFunctionWithArguments "quot" [x, y] _ = Just (quot x y)
evaluateUnsteppableFunctionWithArguments "quotRem" [x, y] _ = Just $ expressionTupleToCoreTuple (quotRem x y)
evaluateUnsteppableFunctionWithArguments "divMod" [x, y] _ = Just $ expressionTupleToCoreTuple (divMod x y)
evaluateUnsteppableFunctionWithArguments "rem" [x, y] _ = Just (rem x y)
evaluateUnsteppableFunctionWithArguments "div" [x, y] _ = Just (div x y)
evaluateUnsteppableFunctionWithArguments "mod" [x, y] _ = Just (mod x y)
evaluateUnsteppableFunctionWithArguments "toInteger" [x] _ = Just (integerToCoreExpression (toInteger x))
evaluateUnsteppableFunctionWithArguments "toRational" [x] _ = Just (rationalToCoreExpression (toRational x))
evaluateUnsteppableFunctionWithArguments "floatRadix" [x] _ = Just $ integerToCoreExpression (floatRadix x)
evaluateUnsteppableFunctionWithArguments "floatDigits" [x] _ = Just $ integerToCoreExpression (toInteger (floatDigits x))
evaluateUnsteppableFunctionWithArguments "floatRange" [x] _ = Just $ expressionTupleToCoreTuple (join bimap integerToCoreExpression (join bimap toInteger (floatRange x)))
evaluateUnsteppableFunctionWithArguments "decodeFloat" [x] _ = Just $ expressionTupleToCoreTuple (integerToCoreExpression (fst res), integerToCoreExpression (toInteger (snd res))) where res = decodeFloat x
evaluateUnsteppableFunctionWithArguments "encodeFloat" [Lit (LitNumber _ x), Lit (LitNumber _ y)] _ = Just (encodeFloat x (fromIntegral y))
evaluateUnsteppableFunctionWithArguments "exponent" [x] _ = Just (integerToCoreExpression (toInteger (exponent x)))
evaluateUnsteppableFunctionWithArguments "significand" [x] _ = Just (significand x)
evaluateUnsteppableFunctionWithArguments "scaleFloat" [Lit (LitNumber _ x), y] _ = Just (scaleFloat (fromIntegral x) y)
evaluateUnsteppableFunctionWithArguments "isNaN" [x] _ = Just (boolToCoreExpression (isNaN x))
evaluateUnsteppableFunctionWithArguments "isInfinite" [x] _ = Just (boolToCoreExpression (isInfinite x))
evaluateUnsteppableFunctionWithArguments "isDenormalized" [x] _ = Just (boolToCoreExpression (isDenormalized x))
evaluateUnsteppableFunctionWithArguments "isNegativeZero" [x] _ = Just (boolToCoreExpression (isNegativeZero x))
evaluateUnsteppableFunctionWithArguments "isIEEE" [x] _ = Just (boolToCoreExpression (isIEEE x))
evaluateUnsteppableFunctionWithArguments "atan2" [x, y] _ = Just (atan2 x y)
evaluateUnsteppableFunctionWithArguments "properFraction" [x] _ = Just $ expressionTupleToCoreTuple (properFraction x)
evaluateUnsteppableFunctionWithArguments "truncate" [x] _ = Just (integerToCoreExpression (toInteger (truncate x)))
evaluateUnsteppableFunctionWithArguments "round" [x] _ = Just (integerToCoreExpression (toInteger (round x)))
evaluateUnsteppableFunctionWithArguments "ceiling" [x] _ = Just (integerToCoreExpression (toInteger (ceiling x)))
evaluateUnsteppableFunctionWithArguments "floor" [x] _ = Just (integerToCoreExpression (toInteger (floor x)))
evaluateUnsteppableFunctionWithArguments "eqString" [x, y] _ = Just (boolToCoreExpression (x == y))
evaluateUnsteppableFunctionWithArguments "fmap" [x, y] _ = customFmapForMaybe x y
evaluateUnsteppableFunctionWithArguments "primError" [x] _ = Nothing
evaluateUnsteppableFunctionWithArguments "error" [x] _ = Nothing
evaluateUnsteppableFunctionWithArguments "seq" [x, y] reducer = Just (seq (reducer x) y)
evaluateUnsteppableFunctionWithArguments "ord" [Lit (LitChar input)] reducer = Just (integerToCoreExpression (toInteger (ord input)))
evaluateUnsteppableFunctionWithArguments "isSpace" [Lit (LitChar input)] reducer = Just (boolToCoreExpression (isSpace input))
evaluateUnsteppableFunctionWithArguments "unsteppableFunction'primIntToChar" [Lit (LitNumber _ input)] reducer = Just (charToCoreExpression (toEnum (fromIntegral input)))
evaluateUnsteppableFunctionWithArguments "unsteppableFunction'primCharToInt" [Lit (LitChar input)] reducer = Just (integerToCoreExpression (toInteger (fromEnum input)))
evaluateUnsteppableFunctionWithArguments functionName [x] reducer | isTypeWrapperFunctionName functionName = Just x
evaluateUnsteppableFunctionWithArguments name args _ = trace (((("function not supported: '" ++ name) ++ "' ") ++ "with argument-lenght: ") ++ show (length args)) Nothing --function not supported

evaluateUnsteppableFunctionWithArgumentsAndTypes :: String -> [Expr Var] -> Reducer -> Maybe (Expr Var)
evaluateUnsteppableFunctionWithArgumentsAndTypes "return" [Type monadType, _, Type ty, value] _ = Just (customReturn monadType ty value)
evaluateUnsteppableFunctionWithArgumentsAndTypes "fail" [Type monadType, _, Type ty, _] _ = Just (customFail monadType ty)
evaluateUnsteppableFunctionWithArgumentsAndTypes ">>=" [_, _, _, Type newType, argument, function] reducer = customMonadOperator newType argument function reducer
evaluateUnsteppableFunctionWithArgumentsAndTypes ">>" [_, _, _, Type newType, argument, function] reducer = customMonadOperator2 newType argument function reducer
evaluateUnsteppableFunctionWithArgumentsAndTypes "fmap" [_, _, _, Type newType, function, argument] _ = customFmapForList newType function argument
evaluateUnsteppableFunctionWithArgumentsAndTypes "minBound" [Type ty, _] _ = minBoundForType ty
evaluateUnsteppableFunctionWithArgumentsAndTypes "maxBound" [Type ty, _] _ = maxBoundForType ty
evaluateUnsteppableFunctionWithArgumentsAndTypes name arguments _ = Nothing

