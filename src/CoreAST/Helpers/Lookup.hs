-- |
-- Module      : CoreLookup
-- Description : Used to find bindings
-- License     : GPL-3
--
-- A CoreProgramm contains a list of top-level bindings. During the stepping, it is
-- sometimes necessary to replace "Var"s that reference a binding with the underlying
-- expression of the binding. This module provides functions to find specific
-- bindings.
module CoreAST.Helpers.Lookup (tryFindExpression, findMatchingPattern, findExpressionForString, tryFindBindingForString) where

import CoreAST.Helpers.Transformer
  ( convertToMultiArgumentFunction,
    deepReplaceMultipleVarWithinExpression,
    deepReplaceVarWithinExpression,
  )
import CoreAST.InformationExtractor
  ( isPrimitiveTypeConstructorName,
    nameToString,
    removeTypeInformation,
    varNameEqualsString,
    varRefersToUnsteppableFunction,
    varToString,
    varsHaveTheSameName,
    varsHaveTheSameType,
  )
import CoreAST.TypeClassInstances ()
import CoreAST.TypeDefinitions
  ( Binding,
    FunctionName,
    FunctionReference,
  )
import Data.List ()
import Data.Maybe (fromMaybe, isJust, isNothing)
import GHC.Plugins
  ( Alt,
    AltCon (DEFAULT, DataAlt, LitAlt),
    CoreExpr,
    Expr (App, Lit, Var),
    Var,
    dataConName,
    trace,
  )
import Utils ()

overrideFunctionPrefix = "override'"

-- | tries to find the corresponding function-expression inside the list of bindings for a given function reference
tryFindExpression :: FunctionReference -> [Binding] -> Maybe CoreExpr
tryFindExpression name bindings = do
  if varRefersToUnsteppableFunction name
    then Nothing --use implementation from stepper backend
    else do
      let overrideBindings = tryFindExpressionForString (overrideFunctionPrefix ++ varToString name) bindings
      if isNothing overrideBindings
        then tryFindBindingForVar name bindings
        else overrideBindings
  where
    tryFindBindingForVar :: FunctionReference -> [Binding] -> Maybe CoreExpr
    tryFindBindingForVar key bindings = tryFindExpressionForCriteriaCascade [equalityFilter, nameAndSignatureFilter] bindings
      where
        equalityFilter binding = (==) (fst binding) key
        nameAndSignatureFilter binding = (&&) (varsHaveTheSameName (fst binding) key) (varsHaveTheSameType (fst binding) key)

-- | searches for the corresponding function-expression inside the list of bindings for a given function name
--  should only be used for automatic testing as it leads to an error if no expression is find
findExpressionForString :: FunctionName -> [Binding] -> CoreExpr
findExpressionForString name bindings = do
  let foundBinding = tryFindExpressionForString name bindings
  fromMaybe (error ("binding not found : " ++ name)) foundBinding

-- | tries to for the corresponding function-expression inside the list of bindings for a given function name
tryFindExpressionForString :: FunctionName -> [Binding] -> Maybe CoreExpr
tryFindExpressionForString key bindings = tryFindExpressionForCriteriaCascade [\binding -> varNameEqualsString (fst binding) key] bindings

-- | tries to find a binding inside the list of bindings for a given function name
tryFindBindingForString :: FunctionName -> [Binding] -> Maybe Binding
tryFindBindingForString key bindings = tryFindBindingForCriteriaCascade [\binding -> varNameEqualsString (fst binding) key] bindings

-- | tries to find a binding
--  the caller provides criteria functions to decide if a binding matches the expectation.
--  a criteria function returns "True" if a binding matches the search-criteria, otherwise "False"
--  If there are multiple bindings that match a criteria, only the first "match" is used
--  If multiple criteria functions are provided, only the first one is used. In no matching
--  binding was found, the second criteria gets used and so on
tryFindBindingForCriteriaCascade :: [Binding -> Bool] -> [Binding] -> Maybe Binding
tryFindBindingForCriteriaCascade [] bindings = Nothing
tryFindBindingForCriteriaCascade (criteria : criterias) bindings = do
  let foundBindings = filter criteria bindings
  case length foundBindings of
    0 -> tryFindBindingForCriteriaCascade criterias bindings
    _ -> Just (head foundBindings)

-- | tries to find function-expression inside the list of bindings.
--  the caller provides criteria functions to decide if a binding matches the expectation.
--  a criteria function returns "True" if a binding matches the search-criteria, otherwise "False"
--  If there are multiple bindings that match a criteria, only the first "match" is used
--  If multiple criteria functions are provided, only the first one is used. In no matching
--  binding was found, the second criteria gets used and so on
tryFindExpressionForCriteriaCascade :: [Binding -> Bool] -> [Binding] -> Maybe CoreExpr
tryFindExpressionForCriteriaCascade criteria bindings = do
  let binding = tryFindBindingForCriteriaCascade criteria bindings
  fmap snd binding

-- | takes a list of pattern alternatives and decides which alternative
--  matches a given expression. Only the underlying expression
--  of the matchin pattern is returned.
findMatchingPattern :: CoreExpr -> [Alt Var] -> Maybe CoreExpr
findMatchingPattern expression patterns = do
  --in Haskell Core, the default pattern is always the first one
  --this means that patterns can not be checked for a "match"
  --from "top to bottom", the default pattern has to be
  --ignored first
  let foundPattern = findMatchingPatternIgnoreDefault expression patterns
  if isJust foundPattern
    then foundPattern
    else findMatchingDefaultPattern expression patterns
  where
    findMatchingPatternIgnoreDefault :: CoreExpr -> [Alt Var] -> Maybe CoreExpr
    findMatchingPatternIgnoreDefault expression [] = Nothing
    findMatchingPatternIgnoreDefault (Var name) ((DataAlt dataCon, _, expression) : xs) =
      --pattern matching on a constructor, for example "True"
      if (==) (varToString name) (nameToString (dataConName dataCon)) --is there a more elegant way than nameToString?
        then Just expression
        else findMatchingPatternIgnoreDefault (Var name) xs
    findMatchingPatternIgnoreDefault (Lit literal) ((LitAlt patternLiteral, _, expression) : xs) =
      --pattern matching on a literal, for example 5
      if (==) (Lit literal) (Lit patternLiteral) --literals are wrapped inside an expression in order to use the custom equality operator for expressions
        then Just expression
        else findMatchingPatternIgnoreDefault (Lit literal) xs
    findMatchingPatternIgnoreDefault (Lit literal) ((DataAlt patternConstructorName, [boundName], expression) : xs) = do
      --pattern matching on a primitive constructor literal, for example I# 1
      if isPrimitiveTypeConstructorName (dataConName patternConstructorName)
        then Just (deepReplaceVarWithinExpression boundName (Lit literal) expression)
        else findMatchingPatternIgnoreDefault (Lit literal) xs
    findMatchingPatternIgnoreDefault (App expr argument) ((DataAlt patternConstructorName, boundNames, expression) : xs) = do
      --pattern matching on a constructor with arguments, for example for lists: (: a b)
      let (Var var, arguments) = convertToMultiArgumentFunction (App expr argument)
      if (==) (varToString var) (nameToString (dataConName patternConstructorName))
        then Just (deepReplaceMultipleVarWithinExpression boundNames (removeTypeInformation arguments) expression)
        else findMatchingPatternIgnoreDefault (App expr argument) xs
    findMatchingPatternIgnoreDefault expression (x : xs) = findMatchingPatternIgnoreDefault expression xs

    findMatchingDefaultPattern :: CoreExpr -> [Alt Var] -> Maybe CoreExpr
    findMatchingDefaultPattern expression [] = trace "no matching pattern found" Nothing
    findMatchingDefaultPattern _ ((DEFAULT, _, expression) : _) = Just expression
    findMatchingDefaultPattern expression (x : xs) = findMatchingDefaultPattern expression xs