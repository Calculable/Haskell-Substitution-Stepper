module OriginalCoreAST.CoreStepperHelpers.CoreEvaluatorHelper (customFmapForMaybe, minBoundForType, maxBoundForType, customMonadOperator, customMonadOperator2, customReturn, customFail, customFmapForList) where

import GHC.Plugins
import OriginalCoreAST.CoreInformationExtractorFunctions
import OriginalCoreAST.CoreMakerFunctions
import Debug.Trace
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator
import Data.Maybe

type Reducer = (Expr Var -> Maybe (Expr Var)) 

customFmapForMaybe :: Expr Var -> Expr Var -> Maybe (Expr Var)
customFmapForMaybe function (App constructor argument)
  | isNothingMaybe (App constructor argument) = Just (App constructor argument)
  | isJustMaybe (App constructor argument) = Just (App constructor (App function argument))
customFmapForMaybe _ _ = trace "fmap not supported for this type" Nothing

minBoundForType :: Type -> Maybe (Expr Var)
minBoundForType ty  | isIntType ty = Just $ integerToCoreExpression (toInteger (minBound::Int))
                    | isBoolType ty = Just $ boolToCoreExpression (minBound::Bool)
                    | isCharType ty = Just $ charToCoreExpression (minBound::Char)
                    | otherwise = Nothing

maxBoundForType :: Type -> Maybe (Expr Var)
maxBoundForType ty  | isIntType ty = Just $ integerToCoreExpression (toInteger (maxBound::Int))
                    | isBoolType ty = Just $ boolToCoreExpression (maxBound::Bool)
                    | isCharType ty = Just $ charToCoreExpression (maxBound::Char)
                    | otherwise = Nothing

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
  if isListType monadType
    then expressionListToCoreListWithType ty [expression]
    else maybeToCoreExpression (Just expression) ty --is maybe type (could be checked again)

customFail :: Type -> Type -> Expr Var
customFail monadType ty =
  if isListType monadType
    then expressionListToCoreListWithType ty []
    else maybeToCoreExpression Nothing ty --is maybe type (could be checked again)

repalceAllListItemsWithFunction :: Type -> Expr Var -> Expr Var -> Maybe (Expr Var)
repalceAllListItemsWithFunction newType element functorArgument
  | isList functorArgument = do
    let listItems = getIndividualElementsOfList functorArgument
    let listItemsWithoutTypes = removeTypeInformation listItems
    let mappedListItems = replicate (length listItemsWithoutTypes) element
    Just (expressionListToCoreListWithType newType mappedListItems)
  | otherwise = Nothing

customFmapForList :: Type -> Expr Var -> Expr Var -> Maybe (Expr Var)
customFmapForList newType function functorArgument
  | isList functorArgument = do
    let listItems = getIndividualElementsOfList functorArgument
    let listItemsWithoutTypes = filter (not . isTypeInformation) listItems
    let mappedListItems = map (App function) listItemsWithoutTypes
    Just (expressionListToCoreListWithType newType mappedListItems)
  | otherwise = Nothing

customConcatForList :: Type -> Expr Var -> Reducer -> Maybe (Expr Var)
customConcatForList newType nestedListExpression reducer = do
  let (_, subLists) = convertToMultiArgumentFunction nestedListExpression
  let maybeMappedArguments = map reducer subLists
  if any isNothing maybeMappedArguments
    then Nothing
    else do
      let flatArguments = concatMap (extractArgumentsOfNestedApplication . fromJust) maybeMappedArguments
      return (expressionListToCoreListWithType newType (filter (not . isTypeInformation) flatArguments))
