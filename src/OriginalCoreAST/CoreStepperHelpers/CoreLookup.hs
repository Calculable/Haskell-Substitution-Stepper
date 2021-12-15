module OriginalCoreAST.CoreStepperHelpers.CoreLookup (tryFindBinding, findMatchingPattern, findBindingForString) where

import Data.Maybe (fromMaybe, isNothing)
import Data.List (isPrefixOf)
import GHC.Plugins
  ( Alt,
    AltCon (DEFAULT, DataAlt, LitAlt),
    Expr (App, Lit, Var),
    Var(), 
    varUnique, 
    varName, 
    varType,
    trace,
  )
import OriginalCoreAST.CoreInformationExtractorFunctions
  ( isTypeInformation,
    varExpressionToString,
    varToString,
  )
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator
  ( convertToMultiArgumentFunction,
    deepReplaceMultipleVarWithinExpression,
  )
import OriginalCoreAST.CoreTypeClassInstances ()
import Utils (showOutputable)

type Binding = (Var, Expr Var) --for example x = 2 (x is "var" and 2 is "expr")

tryFindBinding :: Var -> [Binding] -> Maybe (Expr Var)
tryFindBinding = tryFindBindingIncludingOverrideFunctions

tryFindBindingIncludingOverrideFunctions :: Var -> [Binding] -> Maybe (Expr Var)
tryFindBindingIncludingOverrideFunctions name bindings = do
  if "unsteppableFunction'" `isPrefixOf` varToString name
    then Nothing --use implementation from stepper backend
    else do
      let overrideBindings =  tryFindBindingForString ("override'" ++ varToString name) bindings
      if isNothing overrideBindings
        then tryFindBindingForVar name bindings
        else overrideBindings

--should only be used for testing
findBindingForString :: String -> [Binding] -> Expr Var
findBindingForString name bindings = do
  let foundBinding = tryFindBindingForString name bindings
  fromMaybe (error ("binding not found : " ++ name)) foundBinding

tryFindBindingForVar :: Var -> [Binding] -> Maybe (Expr Var)
tryFindBindingForVar key bindings = tryFindBindingForCriteriaCascade [equalityFilter, nameAndSignatureFilter] bindings
  where
    equalityFilter = (\binding -> (==) (fst binding) key)
    nameAndSignatureFilter = (\binding -> (&&) ((==) (showOutputable (varName (fst binding))) (showOutputable (varName key))) ((==) (showOutputable (varType (fst binding))) (showOutputable (varType key))))

tryFindBindingForString :: String -> [Binding] -> Maybe (Expr Var)
tryFindBindingForString key bindings = tryFindBindingForCriteriaCascade [(\binding -> (==) (varToString (fst binding)) key)] bindings

tryFindBindingForCriteriaCascade :: [(Binding -> Bool)] -> [Binding] -> Maybe (Expr Var)
tryFindBindingForCriteriaCascade [] bindings = Nothing
tryFindBindingForCriteriaCascade (criteria:criterias) bindings = do
  let foundBindings = filter criteria bindings
  case (length foundBindings) of {
    0 -> tryFindBindingForCriteriaCascade criterias bindings;
    1 -> Just (snd (head foundBindings));
    _ -> trace "more than one binding was found. I will return the first one (this might lead to a wrong expression)" Just (snd (head foundBindings))
  }

findMatchingPattern :: Expr Var -> [Alt Var] -> Maybe (Expr Var)
findMatchingPattern expression patterns = do
  let foundPattern = findMatchingPatternIgnoreDefault expression patterns
  case foundPattern of
    (Just x) -> Just x
    Nothing -> findMatchingDefaultPattern expression patterns

findMatchingPatternIgnoreDefault :: Expr Var -> [Alt Var] -> Maybe (Expr Var)
findMatchingPatternIgnoreDefault expression [] = {-trace "no matching pattern found" -} Nothing
findMatchingPatternIgnoreDefault (Var name) ((DataAlt dataCon, _, expression) : xs) =
  if (==) (varToString name) (showOutputable dataCon) --check: is there a more elegant way than "show outputable"
    then Just expression
    else findMatchingPatternIgnoreDefault (Var name) xs
findMatchingPatternIgnoreDefault (Lit literal) ((LitAlt patternLiteral, _, expression) : xs) =
  if (==) literal patternLiteral --can we compare two literals like this?
    then Just expression
    else findMatchingPatternIgnoreDefault (Lit literal) xs
findMatchingPatternIgnoreDefault (App expr argument) ((DataAlt patternConstructorName, boundNames, expression) : xs) = do
  let (function, arguments) = convertToMultiArgumentFunction (App expr argument)
  if (==) (varExpressionToString function) (showOutputable patternConstructorName) --check: is there a more elegant way than "show outputable"
    then Just (deepReplaceMultipleVarWithinExpression boundNames (filter (not . isTypeInformation) arguments) expression)
    else findMatchingPatternIgnoreDefault (App expr argument) xs
findMatchingPatternIgnoreDefault expression (x : xs) = findMatchingPatternIgnoreDefault expression xs

findMatchingDefaultPattern :: Expr Var -> [Alt Var] -> Maybe (Expr Var)
findMatchingDefaultPattern expression [] = {-trace "no matching pattern found"-} Nothing
findMatchingDefaultPattern _ ((DEFAULT, _, expression) : _) = Just expression
findMatchingDefaultPattern expression (x : xs) = findMatchingDefaultPattern expression xs
