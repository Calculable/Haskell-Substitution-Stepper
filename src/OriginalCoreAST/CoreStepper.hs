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


applyStep :: [Binding] -> CoreExpr -> Maybe StepResult
applyStep bindings (Var name) = do
  foundBinding <- tryFindBinding name bindings
  return ("Replace '" ++ varToString name ++ "' with definition", foundBinding, bindings {-replace binding reference with actual expression (Delta Reduction)-})
applyStep bindings (App expr arg) = do
  applyStepToNestedApplication bindings (App expr arg)
applyStep bindings (Case expression binding caseType alternatives) = do
  if canBeReduced expression
    then do
      (description, reducedExpression, newBindings) <- applyStep bindings expression
      return (description, Case reducedExpression binding caseType alternatives, newBindings)
    else do
      matchingPattern <- findMatchingPattern expression alternatives
      return ("Replace with matching pattern", matchingPattern, bindings)
applyStep bindings (Let (NonRec b expr) expression) = do
  Just ("Replace '" ++ varToString b ++ "' with definition", deepReplaceVarWithinExpression b expr expression, bindings)
applyStep bindings (Let (Rec [(b, expr)]) expression) = do
  Just ("Replace '" ++ varToString b ++ "' with definition", deepReplaceVarWithinExpression b expr expression, (b, expr) : bindings)
applyStep bindings (Cast expression cohersion) = do
  Just ("Remove cohersion from cast", expression, bindings)
applyStep bindings (Tick _ _) = do
  trace "no applicable step found: tick is not supported" Nothing
applyStep bindings (Coercion _) = do
  trace "no applicable step found: coercion is not supported" Nothing
applyStep _ _ = trace "no applicable step found" Nothing

applyStepToNestedApplication :: [Binding] -> CoreExpr -> Maybe StepResult
applyStepToNestedApplication bindings expr = do
  let appliedStepWithClassDictionary = tryApplyStepToApplicationUsingClassDictionary bindings expr
  let appliedStepWithoutClassDictionary = tryApplyStepToApplication bindings expr
  if isJust appliedStepWithClassDictionary
    then appliedStepWithClassDictionary
    else appliedStepWithoutClassDictionary
    
tryApplyStepToApplication :: [Binding] -> CoreExpr -> Maybe StepResult
tryApplyStepToApplication bindings expr = do
      let (function, arguments) = convertToMultiArgumentFunction expr
      tryApplyStepToFunctionWithArguments bindings function arguments

tryApplyStepToFunctionWithArguments :: [Binding] -> Function -> [Argument] -> Maybe StepResult
tryApplyStepToFunctionWithArguments bindings (Var var) arguments = do
  if isJust (tryFindBinding var bindings)
    then do
      (description, reducedFunction, newBindings) <- applyStep bindings (Var var)
      return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction arguments, newBindings)
    else do
      evaluateUnsteppableFunction bindings var arguments      
tryApplyStepToFunctionWithArguments bindings (Lam lamdaParameter lamdaExpression) arguments = do
   let reducedFunction = deepReplaceVarWithinExpression lamdaParameter (head arguments) lamdaExpression
   Just ("Lamda Application", convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments), bindings)
tryApplyStepToFunctionWithArguments bindings expression arguments = do
  (description, reducedFunction, newBindings) <- applyStep bindings expression
  return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction arguments, newBindings)

evaluateUnsteppableFunction :: [Binding] -> FunctionReference -> [Argument] -> Maybe StepResult
evaluateUnsteppableFunction bindings function arguments = do
  if any canBeReduced arguments --all arguments are reduced, eval function. This is stric behaviour! We have to use strict behaviour here because we are trying to evaluate a function whose definition we do not know. therefore we cannot apply the arguments one after another but have to simplify all arguments before calling the function
    then do
      (description, simplifiedArguments, newBindings) <- applyStepToOneOfTheArguments bindings [] arguments
      return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication (Var function) simplifiedArguments, newBindings)
    else do
      appliedFunction <- evaluateFunctionWithArguments function arguments (safeReduceToNormalForm bindings) 
      return ("Apply " ++ showOutputable function, appliedFunction, bindings)
                
applyStepToOneOfTheArguments :: [Binding] -> [Argument] -> [Argument] -> Maybe (ReductionStepDescription, [Argument], [Binding])
applyStepToOneOfTheArguments bindings alreadyReducedArguments (x : xs) =
  if canBeReduced x
    then do
      (description, reducedArgument, newBindings) <- applyStep bindings x
      return (description, (alreadyReducedArguments ++ [reducedArgument]) ++ xs, newBindings)
    else applyStepToOneOfTheArguments bindings (alreadyReducedArguments ++ [x]) xs
applyStepToOneOfTheArguments bindings alreadyReducedArguments [] = error "no reducable argument found" --no argument that can be reduced was found. this should not happen because this condition gets checked earlier in the code

tryApplyStepToApplicationUsingClassDictionary :: [Binding] -> CoreExpr -> Maybe StepResult
tryApplyStepToApplicationUsingClassDictionary bindings expr = do
  if isApplicationWithClassDictionary expr
    then do
      let (function, arguments) = convertToMultiArgumentFunction expr
      let (Var functionName) = function
      let typeInformation = head arguments
      let classDictionaryExpression = arguments !! 1
      extractedFunction <- findFunctionInClassDictionary function classDictionaryExpression bindings
      let realFunctionArguments = drop 2 arguments
      let resultExpression = convertFunctionApplicationWithArgumentListToNestedFunctionApplication extractedFunction realFunctionArguments
      return ("replace '" ++ varToString functionName ++ "' with definition from the class dictionary", resultExpression, bindings)
    else Nothing

reduceToNormalForm :: [Binding] -> CoreExpr -> CoreExpr
reduceToNormalForm bindings expression = do
  fromJust (reduceToNormalFormWithMaximumAmountOfReductions (negate 1) bindings expression)

maximumAmoutOfReductionsBeforeError :: Integer
maximumAmoutOfReductionsBeforeError = 99

safeReduceToNormalForm :: [Binding] -> CoreExpr -> Maybe CoreExpr
safeReduceToNormalForm = reduceToNormalFormWithMaximumAmountOfReductions maximumAmoutOfReductionsBeforeError

reduceToNormalFormWithMaximumAmountOfReductions :: Integer -> [Binding] -> CoreExpr -> Maybe CoreExpr
reduceToNormalFormWithMaximumAmountOfReductions 0 _ _ = trace "infinite loop" Nothing
reduceToNormalFormWithMaximumAmountOfReductions maximumAmoutOfReductions bindings expression = do
  expressionInHeadNormalForm <- reduceToHeadNormalForm bindings expression
  if canBeReducedToNormalForm expressionInHeadNormalForm
    then do
      let (function, arguments) = convertToMultiArgumentFunction expressionInHeadNormalForm
      let maybeReducedArguments = map (reduceToNormalFormWithMaximumAmountOfReductions (maximumAmoutOfReductions - 1) bindings) arguments
      if any isNothing maybeReducedArguments
        then Nothing
        else Just $ convertFunctionApplicationWithArgumentListToNestedFunctionApplication function (map fromJust maybeReducedArguments)
    else Just expressionInHeadNormalForm

reduceToHeadNormalForm :: [Binding] -> CoreExpr -> Maybe CoreExpr
reduceToHeadNormalForm bindings expression
  | canBeReduced expression = do
    let reduction = applyStep bindings expression
    case reduction of
      Just (reductionStepDescription, reducedExpression, newBindings) -> reduceToHeadNormalForm newBindings reducedExpression
      Nothing -> trace ("Debug - Here is the expression for which no reduction rule is implemented: " ++ showOutputable expression) Nothing
  | otherwise = Just expression

reduceNestedApplicationToHeadNormalForm :: [Binding] -> CoreExpr -> Maybe CoreExpr --can be removed as soon as canBeReduced detects nested applications where the function is a known var
reduceNestedApplicationToHeadNormalForm bindings expr = do
  let result = reduceNestedApplication bindings expr
  maybe (Just expr) (reduceNestedApplicationToHeadNormalForm bindings) result

reduceNestedApplication :: [Binding] -> CoreExpr -> Maybe CoreExpr --can be removed as soon as canBeReduced detects nested applications where the function is a known var
reduceNestedApplication bindings (App expr arg) = do
  let (Var functionName, arguments) = convertToMultiArgumentFunction (App expr arg)
  reducedFunction <- tryFindBinding functionName bindings
  reduceToHeadNormalForm bindings (convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction arguments)
reduceNestedApplication _ _ = Nothing

findFunctionInClassDictionary :: CoreExpr -> CoreExpr -> [Binding] -> Maybe CoreExpr
findFunctionInClassDictionary (Var function) (Var classDictionary) bindings = do
  classDictionaryDefinition <- tryFindBinding classDictionary bindings
  findFunctionInClassDictionaryDefinition bindings (Var function) classDictionaryDefinition
findFunctionInClassDictionary (Var function) (App expr args) bindings = do
  result <- reduceNestedApplicationToHeadNormalForm bindings (App expr args)
  findFunctionInClassDictionaryDefinition bindings (Var function) result
findFunctionInClassDictionary _ _ _ = Nothing

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

