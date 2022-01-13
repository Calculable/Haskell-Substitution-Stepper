-- |
-- Module      : CoreEvaluatorHelper
-- Description : Helper for the CoreEvaluator module
-- License     : GPL-3
--
-- This module contains helper functions used for the CoreEvaluator module.
-- Most of the the functions are used to add support for the "monad" and "functor"
-- type class for lists as well as to add support for the Bounded typeclass
module CoreAST.Helpers.EvaluatorHelper
  ( minBoundForType,
    maxBoundForType,
    monadOperatorForList,
    monadOperator2ForList,
    returnForList,
    failForList,
    fmapForList,
  )
where

import CoreAST.Helpers.Transformer ()
import CoreAST.InformationExtractor
  ( getIndividualElementsOfList,
    isBoolType,
    isCharType,
    isIntType,
    isList,
    removeTypeInformation,
  )
import CoreAST.MakerFunctions
  ( boolToCoreExpression,
    charToCoreExpression,
    expressionListToCoreListWithType,
    integerToCoreExpression,
  )
import CoreAST.TypeDefinitions (Function, Reducer)
import Data.Maybe (fromJust, isNothing)
import Debug.Trace (trace)
import GHC.Plugins (CoreExpr, Expr (App), Type, trace)

{-Functor and Maybe for List-}

-- | applies a function to every element inside a Core list
fmapForList :: Type -> CoreExpr -> CoreExpr -> Maybe CoreExpr
fmapForList newType function functorArgument
  | isList functorArgument = do
    let listItems = getIndividualElementsOfList functorArgument
    let listItemsWithoutTypes = removeTypeInformation listItems
    let mappedListItems = map (App function) listItemsWithoutTypes
    Just (expressionListToCoreListWithType newType mappedListItems)
  | otherwise = Nothing

-- | applies the >>= operator to a Core list
monadOperatorForList :: Type -> CoreExpr -> CoreExpr -> Reducer -> Maybe CoreExpr
monadOperatorForList newType (App constructor argument) function reducer
  | isList (App constructor argument) = do
    fmappedList <- fmapForList newType function (App constructor argument)
    concatForList newType fmappedList reducer
monadOperatorForList _ _ _ _ = trace ">>= not supported for this type" Nothing

-- | applies the >> operator to a Core list
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

-- | wraps an expression inside a Core list
returnForList :: Type -> Type -> CoreExpr -> CoreExpr
returnForList monadType ty expression = expressionListToCoreListWithType ty [expression]

-- | wraps an expression inside a Core list
failForList :: Type -> Type -> CoreExpr
failForList monadType ty = expressionListToCoreListWithType ty []

-- | concatenates nested Core lists
concatForList :: Type -> CoreExpr -> Reducer -> Maybe CoreExpr
concatForList newType nestedListExpression reducer = do
  let listElements = getIndividualElementsOfList nestedListExpression
  let reducedMaybeListArguments = map reducer listElements

  if any isNothing reducedMaybeListArguments
    then trace "cannot reduce list argument" Nothing
    else do
      let reducedListArguments = map fromJust reducedMaybeListArguments
      let concatenatedElements = concatMap getIndividualElementsOfList reducedListArguments
      Just $ expressionListToCoreListWithType newType (removeTypeInformation concatenatedElements)

{-Bounded Typeclass Helper-}

-- | the minBound for a specific Core type
--  this is a functionality provided by the Bounded type class
minBoundForType :: Type -> Maybe CoreExpr
minBoundForType ty
  | isIntType ty = Just $ integerToCoreExpression (toInteger (minBound :: Int))
  | isBoolType ty = Just $ boolToCoreExpression (minBound :: Bool)
  | isCharType ty = Just $ charToCoreExpression (minBound :: Char)
  | otherwise = Nothing

-- | the maxBound for a specific Core type
--  this is a functionality provided by the Bounded type class
maxBoundForType :: Type -> Maybe CoreExpr
maxBoundForType ty
  | isIntType ty = Just $ integerToCoreExpression (toInteger (maxBound :: Int))
  | isBoolType ty = Just $ boolToCoreExpression (maxBound :: Bool)
  | isCharType ty = Just $ charToCoreExpression (maxBound :: Char)
  | otherwise = Nothing
