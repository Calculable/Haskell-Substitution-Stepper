module OriginalCoreAST.CoreStepperHelpers.CoreEvaluatorHelper (minBoundForType, maxBoundForType, monadOperatorForList, monadOperator2ForList, returnForList, failForList, fmapForList) where

import GHC.Plugins
import OriginalCoreAST.CoreInformationExtractorFunctions
import OriginalCoreAST.CoreMakerFunctions
import Debug.Trace
import OriginalCoreAST.CoreStepperHelpers.CoreTransformer
import Data.Maybe
import OriginalCoreAST.CoreInformationExtractorFunctions (removeTypeInformation)
import OriginalCoreAST.CoreTypeDefinitions

{-Functor and Maybe for List-}

fmapForList :: Type -> CoreExpr -> CoreExpr -> Maybe CoreExpr
fmapForList newType function functorArgument
  | isList functorArgument = do
    let listItems = getIndividualElementsOfList functorArgument
    let listItemsWithoutTypes = removeTypeInformation listItems
    let mappedListItems = map (App function) listItemsWithoutTypes
    Just (expressionListToCoreListWithType newType mappedListItems)
  | otherwise = Nothing


monadOperatorForList :: Type -> CoreExpr -> CoreExpr -> Reducer -> Maybe CoreExpr
monadOperatorForList newType (App constructor argument) function reducer
  | isList (App constructor argument) = do
    fmappedList <- fmapForList newType function (App constructor argument)
    concatForList newType fmappedList reducer
monadOperatorForList _ _ _ _ = trace ">>= not supported for this type" Nothing

monadOperator2ForList :: Type -> CoreExpr -> CoreExpr -> Reducer -> Maybe CoreExpr
monadOperator2ForList newType (App constructor argument) function reducer
  | isList (App constructor argument) = do
    fmappedList <- repalceAllListItemsWithFunction newType function (App constructor argument)
    concatForList newType fmappedList reducer
    where
      repalceAllListItemsWithFunction :: Type -> Function -> CoreExpr -> Maybe CoreExpr
      repalceAllListItemsWithFunction newType function functorArgument
        | isList functorArgument = do
          let listItems = getIndividualElementsOfList functorArgument
          let listItemsWithoutTypes = removeTypeInformation listItems
          let mappedListItems = replicate (length listItemsWithoutTypes) function
          Just (expressionListToCoreListWithType newType mappedListItems)
        | otherwise = Nothing

monadOperator2ForList _ _ _ _ = trace ">> not supported for this type" Nothing




returnForList :: Type -> Type -> CoreExpr -> CoreExpr
returnForList monadType ty expression = do
  if isListType monadType
    then expressionListToCoreListWithType ty [expression]
    else maybeToCoreExpression (Just expression) ty --is maybe type (could be checked again)

failForList :: Type -> Type -> CoreExpr
failForList monadType ty =
  if isListType monadType
    then expressionListToCoreListWithType ty []
    else maybeToCoreExpression Nothing ty --is maybe type (could be checked again)


concatForList :: Type -> CoreExpr -> Reducer -> Maybe CoreExpr
concatForList newType nestedListExpression reducer = do
  let (_, subLists) = convertToMultiArgumentFunction nestedListExpression
  let maybeMappedArguments = map reducer subLists
  if any isNothing maybeMappedArguments
    then Nothing
    else do
      let flatArguments = concatMap (extractArgumentsOfNestedApplication . fromJust) maybeMappedArguments
      return (expressionListToCoreListWithType newType (removeTypeInformation flatArguments))

{-Bounded Typeclass Helper-}
minBoundForType :: Type -> Maybe CoreExpr
minBoundForType ty  | isIntType ty = Just $ integerToCoreExpression (toInteger (minBound::Int))
                    | isBoolType ty = Just $ boolToCoreExpression (minBound::Bool)
                    | isCharType ty = Just $ charToCoreExpression (minBound::Char)
                    | otherwise = Nothing

maxBoundForType :: Type -> Maybe CoreExpr
maxBoundForType ty  | isIntType ty = Just $ integerToCoreExpression (toInteger (maxBound::Int))
                    | isBoolType ty = Just $ boolToCoreExpression (maxBound::Bool)
                    | isCharType ty = Just $ charToCoreExpression (maxBound::Char)
                    | otherwise = Nothing


