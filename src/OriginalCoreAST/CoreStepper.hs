{-|
Module      : CoreStepper
Description : Steps through Core expressions by applying reduction rules
License     : GPL-3

This module contians a set of reduction rules. Expressions can either be reduced until head normal form
or even more until normal form. For each reduction, a description of the reduction is generated
which can be useful for printing.
-}

module OriginalCoreAST.CoreStepper (applyStep, reduceToNormalForm, canBeReducedToNormalForm, safeReduceToNormalForm) where

import Data.Maybe
import GHC.Plugins
import OriginalCoreAST.CoreInformationExtractorFunctions
import OriginalCoreAST.CoreStepperHelpers.CoreEvaluator
import OriginalCoreAST.CoreStepperHelpers.CoreLookup
import OriginalCoreAST.CoreStepperHelpers.CoreTransformer
import Utils
import Data.List
import OriginalCoreAST.CoreTypeDefinitions

-- |the maximum amount of reductions to make for the conversion from head normal form to normal form until an error is shown (infinite-loop prevention) 

maximumAmoutOfReductions :: Integer
maximumAmoutOfReductions = 99


-- |takes an expression and applies one single reduction rule (if possible)
applyStep :: [Binding] -> CoreExpr -> Maybe StepResult
applyStep bindings (Var name) = do
  foundBinding <- tryFindBinding name bindings
  return (DeltaReductionStep name, foundBinding, bindings)
applyStep bindings (App expr arg) = do
  applyStepToNestedApplication bindings (App expr arg) --multi-parameter applications are represented as nested applications in Haskell Core
applyStep bindings (Case expression binding caseType alternatives) = do
  if canBeReduced expression
    then do
      (reductionStep, reducedExpression, newBindings) <- applyStep bindings expression
      return (NestedReduction [CaseExpressionStep, reductionStep], Case reducedExpression binding caseType alternatives, newBindings)
    else do
      matchingPattern <- findMatchingPattern expression alternatives
      return (PatternMatchStep, matchingPattern, bindings)
applyStep bindings (Let (NonRec b expr) expression) = do
  Just (ReplaceLetStep b, deepReplaceVarWithinExpression b expr expression, bindings)
applyStep bindings (Let (Rec [(b, expr)]) expression) = do
  Just (ReplaceLetStep b, deepReplaceVarWithinExpression b expr expression, (b, expr) : bindings)
applyStep bindings (Cast expression cohersion) = do
  Just (RemoveCohersionStep, expression, bindings)
applyStep bindings (Tick _ _) = do
  trace "no applicable step found: tick is not supported" Nothing
applyStep bindings (Coercion _) = do
  trace "no applicable step found: coercion is not supported" Nothing
applyStep _ _ = trace "no applicable step found" Nothing

-- |takes an expression containing a (nested) application and applies one single reduction rule.
-- it is checked if the application makes use of a function which is defined in a class dictionary or not
applyStepToNestedApplication :: [Binding] -> CoreExpr -> Maybe StepResult
applyStepToNestedApplication bindings expr = do
  let appliedStepWithClassDictionary = tryApplyStepToApplicationUsingClassDictionary bindings expr
  let appliedStepWithoutClassDictionary = tryApplyStepToApplication bindings expr
  if isJust appliedStepWithClassDictionary
    then appliedStepWithClassDictionary
    else appliedStepWithoutClassDictionary
    
-- |takes an expression containing a (nested) application and applies one single reduction rule (if possible)
tryApplyStepToApplication :: [Binding] -> CoreExpr -> Maybe StepResult
tryApplyStepToApplication bindings expr = do
      let (function, arguments) = convertToMultiArgumentFunction expr
      tryApplyStepToFunctionWithArguments bindings function arguments
        where
          tryApplyStepToFunctionWithArguments :: [Binding] -> Function -> [Argument] -> Maybe StepResult
          tryApplyStepToFunctionWithArguments bindings (Var var) arguments = do
            if isJust (tryFindBinding var bindings)
              then do --function or operator can be stepped
                (reductionStep, reducedFunction, newBindings) <- applyStep bindings (Var var)
                return (NestedReduction [ApplicationExpressionStep, reductionStep], convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction arguments, newBindings)
              else do --function or operator cannot be stepped
                applyStepToApplicationWithAnUnsteppableFunction bindings var arguments      
          tryApplyStepToFunctionWithArguments bindings (Lam lamdaParameter lamdaExpression) arguments = do
            let reducedFunction = deepReplaceVarWithinExpression lamdaParameter (head arguments) lamdaExpression
            Just (ApplicationStep (head arguments), convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments), bindings)
          tryApplyStepToFunctionWithArguments bindings expression arguments = do
            (reductionStep, reducedFunction, newBindings) <- applyStep bindings expression
            return (NestedReduction [ApplicationExpressionStep, reductionStep], convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction arguments, newBindings)

-- |takes an expression containing a (nested) application. If at least one of the arguments to the function can be 
-- reduced, one argumen gets reduced. Please note that this is strict behaviour. If all arguments are at least in 
-- weak head normal form, the function gets evaluated
applyStepToApplicationWithAnUnsteppableFunction :: [Binding] -> FunctionReference -> [Argument] -> Maybe StepResult
applyStepToApplicationWithAnUnsteppableFunction bindings function arguments = do
  if any canBeReduced arguments --all arguments are reduced, eval function. This is stric behaviour! We have to use strict behaviour here because we are trying to evaluate a function whose definition we do not know. therefore we cannot apply the arguments one after another but have to simplify all arguments before calling the function
    then do --reduce one of the arguments
      (reductionStep, simplifiedArguments, newBindings) <- applyStepToOneOfTheArguments bindings [] arguments
      return (NestedReduction [StrictApplicationArgumentStep, reductionStep], convertFunctionApplicationWithArgumentListToNestedFunctionApplication (Var function) simplifiedArguments, newBindings)
    else do --finally evaluate the function with the eargerly evaluted arguments
      appliedFunction <- evaluateFunctionWithArguments function arguments (safeReduceToNormalForm bindings) 
      return (EvaluationStep function, appliedFunction, bindings)
  where              
    applyStepToOneOfTheArguments :: [Binding] -> [Argument] -> [Argument] -> Maybe (ReductionStepDescription, [Argument], [Binding])
    applyStepToOneOfTheArguments bindings alreadyReducedArguments (x : xs) =
      if canBeReduced x
        then do
          (description, reducedArgument, newBindings) <- applyStep bindings x
          return (description, (alreadyReducedArguments ++ [reducedArgument]) ++ xs, newBindings)
        else applyStepToOneOfTheArguments bindings (alreadyReducedArguments ++ [x]) xs
    applyStepToOneOfTheArguments bindings alreadyReducedArguments [] = error "no reducable argument found" --no argument that can be reduced was found. this should not happen because this condition gets checked earlier in the code

-- |takes an expression containing a (nested) application. If the function used 
-- in the application refers to a class dictionary function, it gets
-- replaced with its underlying definition. Nothing is returned if this is not the case
tryApplyStepToApplicationUsingClassDictionary :: [Binding] -> CoreExpr -> Maybe StepResult
tryApplyStepToApplicationUsingClassDictionary bindings expr = do
  if isApplicationWithClassDictionary expr
    then do
      let (function, arguments) = convertToMultiArgumentFunction expr
      let (Var functionName) = function
      let classDictionaryExpression = arguments !! 1 --the second argument contains the class dictionary, for example "$fEqInteger"
      let classDictionaryName = getClassDictionaryVar classDictionaryExpression
      extractedFunction <- findFunctionInClassDictionary function classDictionaryExpression bindings
      let realFunctionArguments = drop 2 arguments --the first two arguments contain the type information. The other arguments are the input for the function
      let resultExpression = convertFunctionApplicationWithArgumentListToNestedFunctionApplication extractedFunction realFunctionArguments
      return (ClassDictionaryLookupStep functionName classDictionaryName, resultExpression, bindings)
    else Nothing
  where
    findFunctionInClassDictionary :: CoreExpr -> CoreExpr -> [Binding] -> Maybe CoreExpr
    findFunctionInClassDictionary (Var function) (Var classDictionary) bindings = do
      classDictionaryDefinition <- tryFindBinding classDictionary bindings
      findFunctionInClassDictionaryDefinition bindings (Var function) classDictionaryDefinition
    findFunctionInClassDictionary (Var function) (App expr args) bindings = do
      result <- reduceNestedApplicationToHeadNormalForm bindings (App expr args)
      findFunctionInClassDictionaryDefinition bindings (Var function) result
    findFunctionInClassDictionary _ _ _ = Nothing

  --this function is used, for example to find the implementation of a function/operator (such as "==") inside the class dictionary (such as $fEqInteger)
    findFunctionInClassDictionaryDefinition :: [Binding] -> CoreExpr -> CoreExpr -> Maybe CoreExpr 
    findFunctionInClassDictionaryDefinition bindings (Var var) (App dictionaryApplication argument) = do
      let (function, dictionaryArguments) = convertToMultiArgumentFunction (App dictionaryApplication argument)
      findDictionaryFunctionForFunctionName bindings var dictionaryArguments
    findFunctionInClassDictionaryDefinition bindings (Var var) (Lam expr arg) = Just (Lam expr arg)
    findFunctionInClassDictionaryDefinition _ _ _ = Nothing

    findDictionaryFunctionForFunctionName :: [Binding] -> FunctionReference -> [CoreExpr] -> Maybe CoreExpr
    findDictionaryFunctionForFunctionName bindings name functionVariables = do
      foundFunction <- find (functionNameMatchesFunctionFromDictionary name) functionVariables
      case foundFunction of
        (Var function) -> tryFindBinding function bindings
        (App expr arg) -> reduceNestedApplicationToHeadNormalForm bindings (App expr arg)

-- |takes an expression and reduces it until normal form without showing substeps.
-- Please note that this function throws an error if the reduction is not possible.
-- Alternatively there is the "safeReduceToNormalForm" that returns Nothing
-- if reduction to normal form is not possible
reduceToNormalForm :: [Binding] -> CoreExpr -> CoreExpr
reduceToNormalForm bindings expression = do
  fromJust (reduceToNormalFormWithMaximumAmountOfReductions (negate 1) bindings expression)

-- |takes an expression and reduces it until normal form without showing substeps.
-- Nothing is returned if reduction to normal form is not possible or takes too much steps (prevention of infinite loops)
safeReduceToNormalForm :: [Binding] -> CoreExpr -> Maybe CoreExpr
safeReduceToNormalForm = reduceToNormalFormWithMaximumAmountOfReductions maximumAmoutOfReductions

-- |takes an expression and reduces it until normal form without showing substeps.
-- the caller can define how many reduction-steps should be performed until redution is
-- aborted (prevention of infinite loops)
reduceToNormalFormWithMaximumAmountOfReductions :: Integer -> [Binding] -> CoreExpr -> Maybe CoreExpr
reduceToNormalFormWithMaximumAmountOfReductions 0 _ _ = trace "infinite loop" Nothing
reduceToNormalFormWithMaximumAmountOfReductions maximumAmountOfReductionsLeft bindings expression = do
  expressionInHeadNormalForm <- reduceToHeadNormalForm bindings expression
  if canBeReducedToNormalForm expressionInHeadNormalForm
    then do
      let (function, arguments) = convertToMultiArgumentFunction expressionInHeadNormalForm
      let maybeReducedArguments = map (reduceToNormalFormWithMaximumAmountOfReductions (maximumAmountOfReductionsLeft - 1) bindings) arguments
      if any isNothing maybeReducedArguments
        then Nothing
        else Just $ convertFunctionApplicationWithArgumentListToNestedFunctionApplication function (map fromJust maybeReducedArguments)
    else Just expressionInHeadNormalForm

-- |takes an expression and reduces it until head normal form without showing substeps.
reduceToHeadNormalForm :: [Binding] -> CoreExpr -> Maybe CoreExpr
reduceToHeadNormalForm bindings expression
  | canBeReduced expression = do
    let reduction = applyStep bindings expression
    case reduction of
      Just (reductionStepDescription, reducedExpression, newBindings) -> reduceToHeadNormalForm newBindings reducedExpression
      Nothing -> trace ("Debug - Here is the expression for which no reduction rule is implemented: " ++ showOutputable expression) Nothing
  | otherwise = Just expression

-- |takes an expression which representes a nested application and reduces it until head normal form without showing substeps
reduceNestedApplicationToHeadNormalForm :: [Binding] -> CoreExpr -> Maybe CoreExpr --can be removed as soon as canBeReduced detects nested applications where the function is a known var
reduceNestedApplicationToHeadNormalForm bindings expr = do
  let result = reduceNestedApplication bindings expr
  maybe (Just expr) (reduceNestedApplicationToHeadNormalForm bindings) result
  where
    reduceNestedApplication :: [Binding] -> CoreExpr -> Maybe CoreExpr --can be removed as soon as canBeReduced detects nested applications where the function is a known var
    reduceNestedApplication bindings (App expr arg) = do
      let (Var functionName, arguments) = convertToMultiArgumentFunction (App expr arg)
      reducedFunction <- tryFindBinding functionName bindings
      reduceToHeadNormalForm bindings (convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction arguments)
    reduceNestedApplication _ _ = Nothing


