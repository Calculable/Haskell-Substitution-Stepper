{-|
Module      : CoreStepperPrinter
Description : Print step by step reductions for a Core expression
License     : GPL-3

This module uses the CoreStepper to generate a step by step reduction for
a core expression. The intermediate-expressions as well as the substep expressions
are then printed 
-}

module OriginalCoreAST.CoreStepperPrinter
  ( printCoreStepByStepReductionForBinding,
    printCoreStepByStepReductionForEveryBinding,
    convertToBindingsList,
    printCoreStepByStepReductionForSingleExpression,
  )
where

import Data.Maybe
import GHC.Plugins
import OriginalCoreAST.CoreInformationExtractorFunctions
import OriginalCoreAST.CorePrettyPrinter
import OriginalCoreAST.CoreStepper
import OriginalCoreAST.CoreStepperHelpers.CoreTransformer
import OriginalCoreAST.CoreTypeDefinitions
import OriginalCoreAST.CoreStepperHelpers.CoreLookup

type VerbosityLevel = Maybe Integer

-- |takes a list of core bindings (from the user), a list of core bindings (from the prelude) and shows a step-by-step reduction for each binding
printCoreStepByStepReductionForEveryBinding :: Maybe FunctionName -> VerbosityLevel -> Bool -> [CoreBind] -> [CoreBind] -> IO ()
printCoreStepByStepReductionForEveryBinding functionToStep verbosityLevel shouldShowComments userBindings preludeBindings = do
  
  if isJust functionToStep && isJust bindingToStep
    then if isJust bindingToStep
          then printCoreStepByStepReductionForBinding configuration allBindings (fromJust bindingToStep) 
          else do
            putStrLn ("No binding named ' " ++ fromJust functionToStep ++ "' was found. Showing all bindings instead...")
            mapM_ (printCoreStepByStepReductionForBinding configuration allBindings) userBindingsList
    else mapM_ (printCoreStepByStepReductionForBinding configuration allBindings) userBindingsList
  
  where
    configuration = do
      let level = fromMaybe 1 verbosityLevel --verbosity level 1 ist default
      case level of {
        1 -> StepperOutputConfiguration {printingStyle = HaskellStyle, showComments = shouldShowComments, showDeltaReductionStep = False, showLamdaApplicationStep = False, showCaseExpressionStep = False, showReplaceLetStep = False, showRemoveCohersionStep = False, showApplicationExpressionStep = False, showClassDictionaryLookupStep = False};
        2 -> StepperOutputConfiguration {printingStyle = HaskellStyle, showComments = shouldShowComments, showDeltaReductionStep = True, showLamdaApplicationStep = True, showCaseExpressionStep = True, showReplaceLetStep = True, showRemoveCohersionStep = True, showApplicationExpressionStep = True, showClassDictionaryLookupStep = True};
        3 -> StepperOutputConfiguration {printingStyle = CoreStyle, showComments = shouldShowComments, showDeltaReductionStep = False, showLamdaApplicationStep = False, showCaseExpressionStep = False, showReplaceLetStep = False, showRemoveCohersionStep = False, showApplicationExpressionStep = False, showClassDictionaryLookupStep = False};
        4 -> StepperOutputConfiguration {printingStyle = CoreStyle, showComments = shouldShowComments, showDeltaReductionStep = True, showLamdaApplicationStep = True, showCaseExpressionStep = True, showReplaceLetStep = True, showRemoveCohersionStep = True, showApplicationExpressionStep = True, showClassDictionaryLookupStep = True};
        _ -> error "unknown verbosity level. Choose a level between 1 (minimal verbosity) and 4 (very verbose)"
      }

    userBindingsList = convertToBindingsList userBindings
    preludeBindingsList = convertToBindingsList preludeBindings
    allBindings = userBindingsList ++ preludeBindingsList

    bindingToStep = tryFindBindingForString (fromJust functionToStep) allBindings

-- |takes a single core binding and shows a step-by-step reduction of the expression
printCoreStepByStepReductionForBinding :: StepperOutputConfiguration -> [Binding] -> Binding -> IO ()
printCoreStepByStepReductionForBinding configuration bindings (var, exp) = do
  putStr "\n**Reduction of "
  putStr (varToString var)
  putStr "**"
  putStrLn ""
  prettyPrint (printingStyle configuration) exp
  printCoreStepByStepReductionForSingleExpression configuration bindings exp

-- |takes a core expression and shows a step-by-step reduction
printCoreStepByStepReductionForSingleExpression :: StepperOutputConfiguration -> [Binding] -> CoreExpr -> IO ()
printCoreStepByStepReductionForSingleExpression configuration bindings expression = do
  let (stepResults, reductionSuccessfulFlag) = getAllSteps bindings expression
  let filteredStepResults = filter (\(stepDescription, _, _) -> not (shouldFilterOutReductionStepForPrintingStyle (printingStyle configuration) stepDescription)) stepResults
  printStepResultList 0 configuration filteredStepResults reductionSuccessfulFlag

-- |takes a a list of substeps and prints the step-by-step reduction
printStepResultList :: Int -> StepperOutputConfiguration -> [StepResult] -> ReductionSuccessfulFlag -> IO ()
printStepResultList _ _ [] True = putStrLn "\n{- reduction completed successfully -}"
printStepResultList _ _ [] False = putStrLn "\n{- reduction completed: no reduction rule implemented for this expression -}"
printStepResultList amountOfSkippedSteps configuration ((stepDescription, expression, _):xs) successFlat = do
  
  if shouldShowReductionStep configuration stepDescription || null xs
    then do
      if amountOfSkippedSteps > 0 && showComments configuration
        then putStrLn ("\n{- skipping " ++ show amountOfSkippedSteps ++ " substeps -}")
        else putStr ""

      if showComments configuration || shouldAlwaysShow stepDescription
        then putStrLn ("\n{- " ++ show stepDescription ++ " -}")
        else putStrLn ""

      prettyPrint (printingStyle configuration) expression
      printStepResultList 0 configuration xs successFlat
    else
      printStepResultList (amountOfSkippedSteps + 1) configuration xs successFlat

-- |reducdes an expression and makes a record of all sub-steps
getAllSteps :: [Binding] -> CoreExpr -> ([StepResult], ReductionSuccessfulFlag)
getAllSteps bindings expression = do
  if canBeReduced expression
    then do
      let maybeReduction = applyStep bindings expression
      case maybeReduction of
        Just (reductionStep, reducedExpression, newBindings) -> do
          let (stepDescription, reductionSuccessfulFlat) = getAllSteps newBindings reducedExpression
          ((reductionStep, reducedExpression, newBindings) : stepDescription, reductionSuccessfulFlat)
        Nothing -> ([], False)
    else do --try to reduce even more (only for visualization)
       if canBeReducedToNormalForm expression
        then do
          let (function, arguments) = convertToMultiArgumentFunction expression
          let maybeArgumentsInNormalForm = map (safeReduceToNormalForm bindings) arguments
          if any isNothing maybeArgumentsInNormalForm
            then  ([], True) 
            else do
              let argumentsInNormalForm = map fromJust maybeArgumentsInNormalForm
              let result = convertFunctionApplicationWithArgumentListToNestedFunctionApplication function argumentsInNormalForm
              ([(ConstructorArgumentReductionForVisualization, result, [])], True)
        else ([], True)
           

-- |converts a list of (nested/recursive) CoreBinds into one single flat list of bindings
convertToBindingsList :: [CoreBind] -> [Binding]
convertToBindingsList = concatMap convertCoreBindingToBindingList
  where
    convertCoreBindingToBindingList :: CoreBind -> [Binding]
    convertCoreBindingToBindingList (NonRec binding exp) = [(binding, exp)]
    convertCoreBindingToBindingList (Rec bindings) = bindings

-- |filters out specific substeps when the result-output looks exactly like the result-input 
-- this is because when we pretty print an expression in a core like manner, some information (like types) 
-- are not shown. It would not make sense for example to show an application when the argument is a type.
shouldFilterOutReductionStepForPrintingStyle :: PrintingStyle -> ReductionStepDescription -> Bool
shouldFilterOutReductionStepForPrintingStyle HaskellStyle (ApplicationStep argument) = isTypeInformation argument --do not show application of type variables, those informations are not shown when pretty printing haskell-like code
shouldFilterOutReductionStepForPrintingStyle HaskellStyle (EvaluationStep function) = isPrimitiveTypeConstructorName (varName function)
shouldFilterOutReductionStepForPrintingStyle style (NestedReduction nestedReduction) = shouldFilterOutReductionStepForPrintingStyle style (last nestedReduction)
shouldFilterOutReductionStepForPrintingStyle _ _ = False

-- |decides if a reductionstep should be shown, depending on the users configuration
shouldShowReductionStep :: StepperOutputConfiguration -> ReductionStepDescription -> Bool
shouldShowReductionStep _ (EvaluationStep _) = True
shouldShowReductionStep _ PatternMatchStep = True
shouldShowReductionStep _ ConstructorArgumentReductionForVisualization = True
shouldShowReductionStep configuration StrictApplicationArgumentStep = True
shouldShowReductionStep configuration (DeltaReductionStep _) = showDeltaReductionStep configuration
shouldShowReductionStep configuration (ApplicationStep _) = showLamdaApplicationStep configuration
shouldShowReductionStep configuration CaseExpressionStep = showCaseExpressionStep configuration
shouldShowReductionStep configuration (ReplaceLetStep _) = showReplaceLetStep configuration
shouldShowReductionStep configuration RemoveCohersionStep = showRemoveCohersionStep configuration
shouldShowReductionStep configuration ApplicationExpressionStep = showApplicationExpressionStep configuration
shouldShowReductionStep configuration (ClassDictionaryLookupStep _ _) = showClassDictionaryLookupStep configuration
shouldShowReductionStep configuration (NestedReduction nestedReductions) = shouldShowReductionStep configuration (head nestedReductions) && shouldShowReductionStep configuration (last nestedReductions)

-- |decides if a reduction step description should be shown even if the user has disabled comments
shouldAlwaysShow :: ReductionStepDescription -> Bool
shouldAlwaysShow ConstructorArgumentReductionForVisualization = True
shouldAlwaysShow _ = False