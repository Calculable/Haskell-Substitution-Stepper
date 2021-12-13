module OriginalCoreAST.CoreStepperHelpers.CoreLookup (tryFindBinding, findMatchingPattern) where

import GHC.Plugins
  ( Alt,
    AltCon (DEFAULT, DataAlt, LitAlt),
    Expr (App, Lit, Var),
    Var,
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
tryFindBinding key [] = Nothing
tryFindBinding key ((var, exp) : xs) =
  if (==) (varToString var) (varToString key)
    then Just exp
    else tryFindBinding key xs

findMatchingPattern :: Expr Var -> [Alt Var] -> Maybe (Expr Var)
findMatchingPattern expression [] = Nothing
findMatchingPattern _ ((DEFAULT, _, expression) : _) = Just expression
findMatchingPattern (Var name) ((DataAlt dataCon, _, expression) : xs) =
  if (==) (varToString name) (showOutputable dataCon) --check: is there a more elegant way than "show outputable"
    then Just expression
    else findMatchingPattern (Var name) xs
findMatchingPattern (Lit literal) ((LitAlt patternLiteral, _, expression) : xs) =
  if (==) literal patternLiteral --can we compare two literals like this?
    then Just expression
    else findMatchingPattern (Lit literal) xs
findMatchingPattern (App expr argument) ((DataAlt patternConstructorName, boundNames, expression) : xs) = do
  let (function, arguments) = convertToMultiArgumentFunction (App expr argument)
  if (==) (varExpressionToString function) (showOutputable patternConstructorName) --check: is there a more elegant way than "show outputable"
    then Just (deepReplaceMultipleVarWithinExpression boundNames (filter (not . isTypeInformation) arguments) expression)
    else findMatchingPattern (App expr argument) xs
findMatchingPattern expression (x : xs) = findMatchingPattern expression xs
