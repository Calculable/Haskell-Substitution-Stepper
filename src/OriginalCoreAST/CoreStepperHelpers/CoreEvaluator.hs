module OriginalCoreAST.CoreStepperHelpers.CoreEvaluator(evaluateFunctionWithArguments, prepareExpressionArgumentForEvaluation)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.Maybe (isNothing)
import GHC.Core (Expr (..))
import GHC.Types.Literal ( Literal (..))
import GHC.Types.Var (Var)
import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression, expressionListToCoreList, expressionTupleToCoreTuple, maybeToCoreExpression, expressionListToCoreListWithType)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced, isList, isMaybe, isJustMaybe, isNothingMaybe, isListType)
import Utils (showOutputable)
import Debug.Trace(trace)
import Data.Bifunctor (bimap)
import Control.Monad  (join)
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator (convertToMultiArgumentFunction, convertFunctionApplicationWithArgumentListToNestedFunctionApplication)
import GHC.Core.TyCo.Rep (Type)

evaluateFunctionWithArguments :: Expr Var -> [Expr Var] -> (Expr Var -> Expr Var) -> Maybe (Expr Var)
evaluateFunctionWithArguments (Var functionOrOperatorName) arguments reducer = do

    let argumentsWithoutApplications = (map prepareExpressionArgumentForEvaluation arguments)
    let evaluatedWithTypes = evaluateUnsteppableFunctionWithArgumentsAndTypes (varToString functionOrOperatorName) argumentsWithoutApplications reducer
    if (isNothing evaluatedWithTypes) 
        then evaluateUnsteppableFunctionWithArguments (varToString functionOrOperatorName) (filter (not.isTypeInformation) argumentsWithoutApplications) --Precondition: function must be in the form of "var". This is already checked by the function which is calling this function.
        else evaluatedWithTypes
evaluateFunctionWithArguments _ _ _ = (error "function-expression has to be a 'Var'")

prepareExpressionArgumentForEvaluation :: Expr Var -> Expr Var
prepareExpressionArgumentForEvaluation (App (Var id) arg) = case varToString id of
                                                                [_, '#'] ->  arg --type constructor, simply return value
                                                                "unpackCString#" -> arg --argument is simply a String
                                                                _ -> App (Var id) arg
prepareExpressionArgumentForEvaluation x = x

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
evaluateUnsteppableFunctionWithArguments "quotRem" [x, y] = Just $ expressionTupleToCoreTuple (quotRem x y)
evaluateUnsteppableFunctionWithArguments "divMod" [x, y] = Just $ expressionTupleToCoreTuple (divMod x y)
evaluateUnsteppableFunctionWithArguments "rem" [x, y] = Just (rem x y)
evaluateUnsteppableFunctionWithArguments "div" [x, y] = Just (div x y)
evaluateUnsteppableFunctionWithArguments "mod" [x, y] = Just (mod x y)
evaluateUnsteppableFunctionWithArguments "toInteger" [x] = Just (integerToCoreExpression (toInteger x))
evaluateUnsteppableFunctionWithArguments "toRational" [x] = Just (rationalToCoreExpression (toRational x))
evaluateUnsteppableFunctionWithArguments "floatRadix" [x] = Just $ integerToCoreExpression (floatRadix x)
evaluateUnsteppableFunctionWithArguments "floatDigits" [x] = Just $ integerToCoreExpression (toInteger (floatDigits x))
evaluateUnsteppableFunctionWithArguments "floatRange" [x] = Just $ expressionTupleToCoreTuple (join bimap integerToCoreExpression (join bimap toInteger (floatRange x)))
evaluateUnsteppableFunctionWithArguments "decodeFloat" [x] = Just $ expressionTupleToCoreTuple (integerToCoreExpression (fst res), integerToCoreExpression (toInteger (snd res))) where res = decodeFloat x
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
evaluateUnsteppableFunctionWithArguments "properFraction" [x] = Just $ expressionTupleToCoreTuple (properFraction x)
evaluateUnsteppableFunctionWithArguments "truncate" [x] = Just (integerToCoreExpression (toInteger (truncate x)))
evaluateUnsteppableFunctionWithArguments "round" [x] = Just (integerToCoreExpression (toInteger (round x)))
evaluateUnsteppableFunctionWithArguments "ceiling" [x] = Just (integerToCoreExpression (toInteger (ceiling x)))
evaluateUnsteppableFunctionWithArguments "floor" [x] = Just (integerToCoreExpression (toInteger (floor x)))
evaluateUnsteppableFunctionWithArguments "eqString" [x, y] = Just (boolToCoreExpression (x == y))
evaluateUnsteppableFunctionWithArguments "fmap" [x, y] = customFmapForMaybe x y
evaluateUnsteppableFunctionWithArguments name args = trace (((("function not supported: '" ++ name) ++ "'") ++ "with argument-lenght: ") ++ show (length args)) Nothing --function not supported

customFmapForMaybe :: Expr Var -> Expr Var -> Maybe (Expr Var)
customFmapForMaybe function (App constructor argument)
    | isNothingMaybe (App constructor argument) = Just (App constructor argument)
    | isJustMaybe (App constructor argument) = Just (App constructor (App function argument))
customFmapForMaybe _ _ = trace "fmap not supported for this type" Nothing


evaluateUnsteppableFunctionWithArgumentsAndTypes :: String -> [Expr Var] -> (Expr Var -> Expr Var) -> Maybe (Expr Var)
evaluateUnsteppableFunctionWithArgumentsAndTypes "return" [(Type monadType), _, (Type ty), value] reducer = Just (customReturn monadType ty value)
evaluateUnsteppableFunctionWithArgumentsAndTypes "fail" [_, _, (Type ty), _] reducer = Just (customFail ty)
evaluateUnsteppableFunctionWithArgumentsAndTypes ">>=" [_, _, _, (Type newType), argument, function] reducer = customMonadOperator newType argument function reducer
evaluateUnsteppableFunctionWithArgumentsAndTypes "fmap" [_, _, _, (Type newType), function, argument] reducer = (customFmapForList newType function argument)

evaluateUnsteppableFunctionWithArgumentsAndTypes name arguments _ = trace (((("typed function not supported: '" ++ name) ++ "'") ++ "with argument-lenght: ") ++ show (length arguments))  Nothing --function not supported


customMonadOperator :: Type -> Expr Var -> Expr Var -> (Expr Var -> Expr Var) -> Maybe (Expr Var)
customMonadOperator newType (App constructor argument) function reducer
    | isNothingMaybe (App constructor argument) = Just (App constructor argument)
    | isJustMaybe (App constructor argument) = Just (App function argument)
    | isList (App constructor argument) = do
        fmappedList <- customFmapForList newType function (App constructor argument)
        return (customConcatForList newType fmappedList reducer)
customMonadOperator _ _ _ _ = trace ">>= not supported for this type" Nothing

customReturn :: Type -> Type -> Expr Var -> Expr Var
customReturn monadType ty expression = do
    if isListType (Type monadType)
        then expressionListToCoreListWithType ty [expression]
        else maybeToCoreExpression (Just expression) ty --is maybe type (could be checked again)    

customFail :: Type -> Expr Var
customFail ty = maybeToCoreExpression Nothing ty

customFmapForList :: Type -> Expr Var -> Expr Var -> Maybe (Expr Var)
customFmapForList newType function functorArgument
    | isList functorArgument = do
        let (_, listItems) = convertToMultiArgumentFunction functorArgument
        let listItemsWithoutTypes = (filter (not.isTypeInformation) listItems)
        let mappedListItems = map (App function) listItemsWithoutTypes
        Just (expressionListToCoreListWithType newType mappedListItems)
    | otherwise = Nothing

customConcatForList :: Type -> Expr Var -> (Expr Var -> Expr Var) -> Expr Var
customConcatForList newType nestedListExpression reducer = do
    let (_, subLists) = convertToMultiArgumentFunction nestedListExpression
    let flatArguments = concatMap extractArgumentsOfNestedApplication (map reducer subLists)
    expressionListToCoreListWithType newType (filter (not.isTypeInformation) flatArguments)

extractArgumentsOfNestedApplication :: Expr Var -> [Expr Var]
extractArgumentsOfNestedApplication expr = snd (convertToMultiArgumentFunction expr)