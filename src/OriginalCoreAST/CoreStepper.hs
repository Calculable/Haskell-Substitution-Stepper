module OriginalCoreAST.CoreStepper (applyStep) where

import Data.List ()
import Data.Maybe (fromJust, isNothing)
import GHC.Plugins
  ( Bind (NonRec),
    Expr (App, Case, Lam, Let, Var),
    Var,
  )
import OriginalCoreAST.CoreInformationExtractorFunctions
  ( canBeReduced,
    varToString,
  )
import OriginalCoreAST.CoreMakerFunctions ()
import OriginalCoreAST.CoreStepperHelpers.CoreEvaluator
  ( evaluateFunctionWithArguments,
  )
import OriginalCoreAST.CoreStepperHelpers.CoreLookup
  ( findMatchingPattern,
    tryFindBinding,
  )
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator
  ( convertFunctionApplicationWithArgumentListToNestedFunctionApplication,
    convertToMultiArgumentFunction,
    deepReplaceVarWithinExpression,
  )
import OriginalCoreAST.CoreTypeClassInstances ()
import Utils (showOutputable)

type ReductionStepDescription = String --for example: "replace x with definition"

type Binding = (Var, Expr Var)

applyStep :: [Binding] -> Expr Var -> Maybe (ReductionStepDescription, Expr Var)
applyStep bindings (Var name) = do
  foundBinding <- tryFindBinding name bindings
  return ("Replace '" ++ varToString name ++ "' with definition", foundBinding {-replace binding reference with actual expression (Delta Reduction)-})
applyStep bindings (App (Lam parameter expression) argument) = do
  Just ("Lamda Application", deepReplaceVarWithinExpression parameter argument expression)
applyStep bindings (App (App first second) third) = do
  applyStepToNestedApp bindings (App (App first second) third) --nested app
applyStep bindings (App (Var name) argument) = do
  let expression = tryFindBinding name bindings
  if isNothing expression
    then do
      applyStepToNestedApp bindings (App (Var name) argument)
    else Just ("Replace '" ++ varToString name ++ "' with definition", App (fromJust expression) argument)
applyStep bindings (Case expression binding caseType alternatives) = do
  if canBeReduced expression
    then do
      (description, reducedExpression) <- applyStep bindings expression
      return (description, Case reducedExpression binding caseType alternatives)
    else do
      matchingPattern <- findMatchingPattern expression alternatives
      return ("Replace with matching pattern", matchingPattern)
applyStep bindings (Let (NonRec b expr) expression) = Just ("Replace '" ++ varToString b ++ "' with definition", deepReplaceVarWithinExpression b expr expression)
applyStep _ _ = do
  Nothing

applyStepToNestedApp :: [Binding] -> Expr Var -> Maybe (ReductionStepDescription, Expr Var)
applyStepToNestedApp bindings expr = do
  let (function, arguments) = convertToMultiArgumentFunction expr
  case function of
    (Var var) ->
      if isNothing (tryFindBinding var bindings)
        then
          if any canBeReduced arguments
            then do
              (description, simplifiedArguments) <- applyStepToOneOfTheArguments bindings [] arguments
              return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication function simplifiedArguments)
            else do
              appliedFunction <- evaluateFunctionWithArguments function arguments --all arguments are reduced, eval function. This is stric behaviour! We have to use strict behaviour here because we are trying to evaluate a function whose definition we do not know. therefor we cannot apply the arguments one after another but have to simplify all arguments before calling the function
              return ("Apply " ++ showOutputable function, appliedFunction)
        else do
          (description, reducedFunction) <- applyStep bindings (App function (head arguments))
          return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments))
    (Lam _ _) -> do
      (description, reducedFunction) <- applyStep bindings (App function (head arguments))
      return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments))

applyStepToOneOfTheArguments :: [Binding] -> [Expr Var] -> [Expr Var] -> Maybe (ReductionStepDescription, [Expr Var])
applyStepToOneOfTheArguments bindings alreadyReducedArguments (x : xs) =
  if canBeReduced x
    then do
      (description, reducedArgument) <- applyStep bindings x
      return (description, (alreadyReducedArguments ++ [reducedArgument]) ++ xs)
    else applyStepToOneOfTheArguments bindings (alreadyReducedArguments ++ [x]) xs
applyStepToOneOfTheArguments bindings alreadyReducedArguments [] = error "no reducable argument found" --no argument that can be reduced was found. this should not happen because this condition gets checked earlier in the code
