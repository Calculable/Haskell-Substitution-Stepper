{-|
Module      : CoreLookup
Description : Used to find bindings
License     : GPL-3

A CoreProgramm contains a list of top-level bindings. During the stepping, it is
sometimes necessary to replace "Var"s that reference a binding with the underlying 
expression of the binding. This module provides functions to find specific
bindings.
-}

module OriginalCoreAST.CoreStepperHelpers.CoreLookup (tryFindBinding, findMatchingPattern, findBindingForString) where
import GHC.Plugins
import Data.List
import OriginalCoreAST.CoreInformationExtractorFunctions
import Data.Maybe
import Utils
import OriginalCoreAST.CoreStepperHelpers.CoreTransformer

import OriginalCoreAST.CoreTypeClassInstances
import OriginalCoreAST.CoreTypeDefinitions

overrideFunctionPrefix = "override'"

-- |tries to find the corresponding function-expression inside the list of bindings for a given function reference
tryFindBinding :: FunctionReference -> [Binding] -> Maybe CoreExpr
tryFindBinding name bindings = do
  if varRefersToUnsteppableFunction name
    then Nothing --use implementation from stepper backend
    else do
      let overrideBindings = tryFindBindingForString (overrideFunctionPrefix ++ varToString name) bindings
      if isNothing overrideBindings
        then tryFindBindingForVar name bindings
        else overrideBindings
  where
    tryFindBindingForVar :: FunctionReference -> [Binding] -> Maybe CoreExpr
    tryFindBindingForVar key bindings = tryFindBindingForCriteriaCascade [equalityFilter, nameAndSignatureFilter] bindings
      where
        equalityFilter binding = (==) (fst binding) key
        nameAndSignatureFilter binding = (&&) (varsHaveTheSameName (fst binding) key) (varsHaveTheSameType (fst binding) key)     

-- |searches for the corresponding function-expression inside the list of bindings for a given function name
-- should only be used for automatic testing as it leads to an error if no expression is find
findBindingForString :: FunctionName -> [Binding] -> CoreExpr
findBindingForString name bindings = do
  let foundBinding = tryFindBindingForString name bindings
  fromMaybe (error ("binding not found : " ++ name)) foundBinding

-- |tries to for the corresponding function-expression inside the list of bindings for a given function name
tryFindBindingForString :: FunctionName -> [Binding] -> Maybe CoreExpr
tryFindBindingForString key bindings = tryFindBindingForCriteriaCascade [(\binding -> varNameEqualsString (fst binding) key)] bindings

-- |tries to find function-expression inside the list of bindings.
-- the caller provides criteria functions to decide if a binding matches the expectation.
-- a criteria function returns "True" if a binding matches the search-criteria, otherwise "False"
-- If there are multiple bindings that match a criteria, only the first "match" is used
-- If multiple criteria functions are provided, only the first one is used. In no matching 
-- binding was found, the second criteria gets used and so on
tryFindBindingForCriteriaCascade :: [Binding -> Bool] -> [Binding] -> Maybe CoreExpr
tryFindBindingForCriteriaCascade [] bindings = Nothing
tryFindBindingForCriteriaCascade (criteria:criterias) bindings = do
  let foundBindings = filter criteria bindings
  case length foundBindings of {
    0 -> tryFindBindingForCriteriaCascade criterias bindings;
    _ -> Just (snd (head foundBindings));
  }

-- |takes a list of pattern alternatives and decides which alternative
-- matches a given expression. Only the underlying expression
-- of the matchin pattern is returned.
findMatchingPattern :: CoreExpr -> [Alt Var] -> Maybe CoreExpr
findMatchingPattern expression patterns = do
  let foundPattern = findMatchingPatternIgnoreDefault expression patterns
  if isJust foundPattern
    then foundPattern
    else findMatchingDefaultPattern expression patterns
  where
    findMatchingPatternIgnoreDefault :: CoreExpr -> [Alt Var] -> Maybe CoreExpr
    findMatchingPatternIgnoreDefault expression [] = Nothing
    findMatchingPatternIgnoreDefault (Var name) ((DataAlt dataCon, _, expression) : xs) =
      if (==) (varToString name) (nameToString (dataConName dataCon)) --is there a more elegant way than nameToString
        then Just expression
        else findMatchingPatternIgnoreDefault (Var name) xs
    findMatchingPatternIgnoreDefault (Lit literal) ((LitAlt patternLiteral, _, expression) : xs) =
      if (==) (Lit literal) (Lit patternLiteral)
        then Just expression
        else findMatchingPatternIgnoreDefault (Lit literal) xs
    findMatchingPatternIgnoreDefault (Lit literal) ((DataAlt patternConstructorName, [boundName], expression) : xs) = do
      if isPrimitiveTypeConstructorName (dataConName patternConstructorName)
        then Just (deepReplaceVarWithinExpression boundName (Lit literal) expression)
        else findMatchingPatternIgnoreDefault (Lit literal) xs
    findMatchingPatternIgnoreDefault (App expr argument) ((DataAlt patternConstructorName, boundNames, expression) : xs) = do
      let (Var var, arguments) = convertToMultiArgumentFunction (App expr argument)
      if (==) (varToString var) (nameToString (dataConName patternConstructorName))
        then Just (deepReplaceMultipleVarWithinExpression boundNames (removeTypeInformation arguments) expression)
            else findMatchingPatternIgnoreDefault (App expr argument) xs
    findMatchingPatternIgnoreDefault expression (x : xs) = findMatchingPatternIgnoreDefault expression xs

    findMatchingDefaultPattern :: CoreExpr -> [Alt Var] -> Maybe CoreExpr
    findMatchingDefaultPattern expression [] = trace "no matching pattern found" Nothing
    findMatchingDefaultPattern _ ((DEFAULT, _, expression) : _) = Just expression
    findMatchingDefaultPattern expression (x : xs) = findMatchingDefaultPattern expression xs