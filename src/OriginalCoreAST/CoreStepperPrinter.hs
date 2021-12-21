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


-- |takes a list of core bindings and shows a step-by-step reduction for each binding
printCoreStepByStepReductionForEveryBinding :: StepperOutputConfiguration -> [CoreBind] -> IO ()
printCoreStepByStepReductionForEveryBinding configuration bindings = do
  let allBindings = convertToBindingsList bindings
  mapM_ (printCoreStepByStepReductionForBinding configuration allBindings) allBindings

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

  let stepResultsExceptLast = if null stepResults
                                then []
                                else init stepResults
  let stepResultsLast = ([last stepResults | not (null stepResults)])

  let filteredStepResults = (filter (\(stepDescription, _, _) -> not (shouldFilterOutReductionStepForPrintingStyle (printingStyle configuration) stepDescription)) stepResultsExceptLast) ++ stepResultsLast 
  printStepResultList 0 configuration filteredStepResults reductionSuccessfulFlag

printStepResultList :: Int -> StepperOutputConfiguration -> [StepResult] -> ReductionSuccessfulFlag -> IO ()
printStepResultList _ _ [] True = putStrLn "{- reduction completed successfully -}"
printStepResultList _ _ [] False = putStrLn "{- reduction completed: no reduction rule implemented for this expression -}"
printStepResultList amountOfSkippedSteps configuration ((stepDescription, expression, _):xs) successFlat = do
  
  if shouldShowReductionStep configuration stepDescription || null xs
    then do
      if amountOfSkippedSteps > 0
        then putStrLn ("{- skipping " ++ show amountOfSkippedSteps ++ " substeps -}")
        else putStr ""
      putStrLn ("{- " ++ show stepDescription ++ " -}")
      prettyPrint (printingStyle configuration) expression
      printStepResultList 0 configuration xs successFlat
    else
      printStepResultList (amountOfSkippedSteps + 1) configuration xs successFlat

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


shouldFilterOutReductionStepForPrintingStyle :: PrintingStyle -> ReductionStepDescription -> Bool
shouldFilterOutReductionStepForPrintingStyle HaskellStyle (ApplicationStep argument) = isTypeInformation argument --do not show application of type variables, those informations are not shown when pretty printing haskell-like code
shouldFilterOutReductionStepForPrintingStyle HaskellStyle (EvaluationStep function) = isPrimitiveTypeConstructorName (varName function)
shouldFilterOutReductionStepForPrintingStyle style (NestedReduction nestedReduction) = shouldFilterOutReductionStepForPrintingStyle style (last nestedReduction)
shouldFilterOutReductionStepForPrintingStyle _ _ = False

shouldShowReductionStep :: StepperOutputConfiguration -> ReductionStepDescription -> Bool
shouldShowReductionStep _ (EvaluationStep _) = True
shouldShowReductionStep _ PatternMatchStep = True
shouldShowReductionStep _ ConstructorArgumentReductionForVisualization = True
shouldShowReductionStep configuration _ = False
