module OriginalCoreAST.CoreStepperHelpers.CoreEvaluator(evaluateFunctionWithArguments, prepareExpressionArgumentForEvaluation)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.Maybe (isNothing, fromJust)
import GHC.Core (Expr (..))
import GHC.Types.Literal ( Literal (..))
import GHC.Types.Var (Var)
import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression, expressionListToCoreList, expressionTupleToCoreTuple, maybeToCoreExpression, expressionListToCoreListWithType)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced, isList, isMaybe, isJustMaybe, isNothingMaybe, isListType)
import Utils (showOutputable)
import Debug.Trace(trace)
import Data.Bifunctor (bimap)
import Control.Monad  (join)
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator (convertToMultiArgumentFunction, convertFunctionApplicationWithArgumentListToNestedFunctionApplication, getIndividualElementsOfList)
import GHC.Core.TyCo.Rep (Type)

type Reducer = (Expr Var -> Maybe (Expr Var)) 

evaluateFunctionWithArguments :: Expr Var -> [Expr Var] -> Reducer -> Maybe (Expr Var)
evaluateFunctionWithArguments (Var functionOrOperatorName) arguments reducer = do
    let argumentsWithoutApplications = (map prepareExpressionArgumentForEvaluation arguments)
    let evaluatedWithTypes = evaluateUnsteppableFunctionWithArgumentsAndTypes (varToString functionOrOperatorName) argumentsWithoutApplications reducer
    if (isNothing evaluatedWithTypes) 
        then evaluateUnsteppableFunctionWithArguments (varToString functionOrOperatorName) (filter (not.isTypeInformation) argumentsWithoutApplications) reducer --Precondition: function must be in the form of "var". This is already checked by the function which is calling this function.
        else evaluatedWithTypes
evaluateFunctionWithArguments _ _ _ = (error "function-expression has to be a 'Var'")

prepareExpressionArgumentForEvaluation :: Expr Var -> Expr Var
prepareExpressionArgumentForEvaluation (App (Var id) arg) = case varToString id of
                                                                [_, '#'] ->  arg --type constructor, simply return value
                                                                "unpackCString#" -> arg --argument is simply a String
                                                                _ -> App (Var id) arg
prepareExpressionArgumentForEvaluation x = x

evaluateUnsteppableFunctionWithArguments :: String -> [Expr Var] -> Reducer -> Maybe (Expr Var)
evaluateUnsteppableFunctionWithArguments "+" [x, y] _ = Just ((+) x y)
evaluateUnsteppableFunctionWithArguments "-" [x, y] _ = Just ((-) x y)
evaluateUnsteppableFunctionWithArguments "*" [x, y] _ = Just ((*) x y)
evaluateUnsteppableFunctionWithArguments "/" [x, y] _ = Just ((/) x y)
evaluateUnsteppableFunctionWithArguments "recip" [x] _ = Just (recip x)
evaluateUnsteppableFunctionWithArguments "signum" [x] _ = Just (signum x)
evaluateUnsteppableFunctionWithArguments "abs" [x] _ = Just (abs x)
evaluateUnsteppableFunctionWithArguments "negate" [x] _ = Just (negate x)
evaluateUnsteppableFunctionWithArguments "fromInteger" [(Lit (LitNumber _ x))] _ = Just (fromInteger x)
evaluateUnsteppableFunctionWithArguments "/=" [x, y] _ = Just (boolToCoreExpression ((/=) x y))
evaluateUnsteppableFunctionWithArguments "==" [x, y] _ = Just (boolToCoreExpression ((==) x y))
evaluateUnsteppableFunctionWithArguments "<" [x, y] _ = Just (boolToCoreExpression ((<) x y))
evaluateUnsteppableFunctionWithArguments ">" [x, y] _ = Just (boolToCoreExpression ((>) x y))
evaluateUnsteppableFunctionWithArguments ">=" [x, y] _ = Just (boolToCoreExpression ((>=) x y))
evaluateUnsteppableFunctionWithArguments "<=" [x, y] _ = Just (boolToCoreExpression ((<=) x y))
evaluateUnsteppableFunctionWithArguments "min" [x, y] _ = Just $ min x y
evaluateUnsteppableFunctionWithArguments "max" [x, y] _ = Just $ max x y
evaluateUnsteppableFunctionWithArguments "unpackCString#" [x] _ = Just x
evaluateUnsteppableFunctionWithArguments "succ" [x] _ = Just $ succ x
evaluateUnsteppableFunctionWithArguments "pred" [x] _ = Just $ pred x
evaluateUnsteppableFunctionWithArguments "fromEnum" [x] _ = Just $ integerToCoreExpression  (toInteger (fromEnum x))
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
evaluateUnsteppableFunctionWithArguments "encodeFloat" [(Lit (LitNumber _ x)), (Lit (LitNumber _ y))] _ = Just (encodeFloat x (fromIntegral y))
evaluateUnsteppableFunctionWithArguments "exponent" [x] _ = Just (integerToCoreExpression (toInteger (exponent x)))
evaluateUnsteppableFunctionWithArguments "significand" [x] _ = Just (significand x)
evaluateUnsteppableFunctionWithArguments "scaleFloat" [(Lit (LitNumber _ x)), y] _ = Just (scaleFloat (fromIntegral x) y)
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

evaluateUnsteppableFunctionWithArguments name args _ = trace (((("function not supported: '" ++ name) ++ "' ") ++ "with argument-lenght: ") ++ show (length args)) Nothing --function not supported

customFmapForMaybe :: Expr Var -> Expr Var -> Maybe (Expr Var)
customFmapForMaybe function (App constructor argument)
    | isNothingMaybe (App constructor argument) = Just (App constructor argument)
    | isJustMaybe (App constructor argument) = Just (App constructor (App function argument))
customFmapForMaybe _ _ = trace "fmap not supported for this type" Nothing


evaluateUnsteppableFunctionWithArgumentsAndTypes :: String -> [Expr Var] -> Reducer -> Maybe (Expr Var)
evaluateUnsteppableFunctionWithArgumentsAndTypes "return" [(Type monadType), _, (Type ty), value] reducer = Just (customReturn monadType ty value)
evaluateUnsteppableFunctionWithArgumentsAndTypes "fail" [(Type monadType), _, (Type ty), _] reducer = Just (customFail monadType ty)
evaluateUnsteppableFunctionWithArgumentsAndTypes ">>=" [_, _, _, (Type newType), argument, function] reducer = customMonadOperator newType argument function reducer
evaluateUnsteppableFunctionWithArgumentsAndTypes ">>" [_, _, _, (Type newType), argument, function] reducer = customMonadOperator2 newType argument function reducer
evaluateUnsteppableFunctionWithArgumentsAndTypes "fmap" [_, _, _, (Type newType), function, argument] reducer = (customFmapForList newType function argument)

evaluateUnsteppableFunctionWithArgumentsAndTypes name arguments _ = Nothing --function not supported


customMonadOperator :: Type -> Expr Var -> Expr Var -> Reducer -> Maybe (Expr Var)
customMonadOperator newType (App constructor argument) function reducer
    | isNothingMaybe (App constructor argument) = Just (App constructor argument)
    | isJustMaybe (App constructor argument) = Just (App function argument)
    | isList (App constructor argument) = do
        fmappedList <- customFmapForList newType function (App constructor argument)
        customConcatForList newType fmappedList reducer
customMonadOperator _ _ _ _ = trace ">>= not supported for this type" Nothing

customMonadOperator2 :: Type -> Expr Var -> Expr Var -> Reducer -> Maybe (Expr Var)
customMonadOperator2 newType (App constructor argument) function reducer
    | isNothingMaybe (App constructor argument) = Just (App constructor argument)
    | isJustMaybe (App constructor argument) = Just function
    | isList (App constructor argument) = do
        fmappedList <- repalceAllListItemsWithFunction newType function (App constructor argument)
        customConcatForList newType fmappedList reducer
customMonadOperator2 _ _ _ _ = trace ">> not supported for this type" Nothing

customReturn :: Type -> Type -> Expr Var -> Expr Var
customReturn monadType ty expression = do
    if isListType (Type monadType)
        then expressionListToCoreListWithType ty [expression]
        else maybeToCoreExpression (Just expression) ty --is maybe type (could be checked again)    

customFail :: Type -> Type -> Expr Var
customFail monadType ty =
    if isListType (Type monadType)
        then expressionListToCoreListWithType ty []
        else maybeToCoreExpression Nothing ty --is maybe type (could be checked again)   

repalceAllListItemsWithFunction :: Type -> Expr Var -> Expr Var -> Maybe (Expr Var)
repalceAllListItemsWithFunction newType element functorArgument
    | isList functorArgument = do
        let listItems = getIndividualElementsOfList functorArgument
        let listItemsWithoutTypes = (filter (not.isTypeInformation) listItems)
        let mappedListItems = replicate (length listItemsWithoutTypes) element
        Just (expressionListToCoreListWithType newType mappedListItems)
    | otherwise = Nothing


customFmapForList :: Type -> Expr Var -> Expr Var -> Maybe (Expr Var)
customFmapForList newType function functorArgument
    | isList functorArgument = do
        let listItems = getIndividualElementsOfList functorArgument
        let listItemsWithoutTypes = (filter (not.isTypeInformation) listItems)
        let mappedListItems = map (App function) listItemsWithoutTypes
        Just (expressionListToCoreListWithType newType mappedListItems)
    | otherwise = Nothing

customConcatForList :: Type -> Expr Var -> Reducer -> Maybe (Expr Var)
customConcatForList newType nestedListExpression reducer = do
    let (_, subLists) = convertToMultiArgumentFunction nestedListExpression
    let maybeMappedArguments = (map reducer subLists)
    if (any isNothing maybeMappedArguments)
        then Nothing
        else do
            let flatArguments = concatMap extractArgumentsOfNestedApplication (map fromJust maybeMappedArguments)
            return (expressionListToCoreListWithType newType (filter (not.isTypeInformation) flatArguments))

extractArgumentsOfNestedApplication :: Expr Var -> [Expr Var]
extractArgumentsOfNestedApplication expr = snd (convertToMultiArgumentFunction expr)