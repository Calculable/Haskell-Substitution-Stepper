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

tryFindBinding :: FunctionReference -> [Binding] -> Maybe CoreExpr
tryFindBinding name bindings = do
  if varRefersToUnsteppableFunction name
    then Nothing --use implementation from stepper backend
    else do
      let overrideBindings = tryFindBindingForString (overrideFunctionPrefix ++ varToString name) bindings
      if isNothing overrideBindings
        then tryFindBindingForVar name bindings
        else overrideBindings

--should only be used for automatic testing
findBindingForString :: FunctionName -> [Binding] -> CoreExpr
findBindingForString name bindings = do
  let foundBinding = tryFindBindingForString name bindings
  fromMaybe (error ("binding not found : " ++ name)) foundBinding

tryFindBindingForVar :: FunctionReference -> [Binding] -> Maybe CoreExpr
tryFindBindingForVar key bindings = tryFindBindingForCriteriaCascade [equalityFilter, nameAndSignatureFilter] bindings
  where
    equalityFilter binding = (==) (fst binding) key
    nameAndSignatureFilter binding = (&&) (varsHaveTheSameName (fst binding) key) (varsHaveTheSameType (fst binding) key) 
      
tryFindBindingForString :: FunctionName -> [Binding] -> Maybe CoreExpr
tryFindBindingForString key bindings = tryFindBindingForCriteriaCascade [(\binding -> varNameEqualsString (fst binding) key)] bindings

tryFindBindingForCriteriaCascade :: [Binding -> Bool] -> [Binding] -> Maybe CoreExpr
tryFindBindingForCriteriaCascade [] bindings = Nothing
tryFindBindingForCriteriaCascade (criteria:criterias) bindings = do
  let foundBindings = filter criteria bindings
  case length foundBindings of {
    0 -> tryFindBindingForCriteriaCascade criterias bindings;
    _ -> Just (snd (head foundBindings));
  }

findMatchingPattern :: CoreExpr -> [Alt Var] -> Maybe CoreExpr
findMatchingPattern expression patterns = do
  let foundPattern = findMatchingPatternIgnoreDefault expression patterns
  if isJust foundPattern
    then foundPattern
    else findMatchingDefaultPattern expression patterns

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