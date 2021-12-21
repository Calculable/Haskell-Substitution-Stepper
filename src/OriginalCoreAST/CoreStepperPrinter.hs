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
printCoreStepByStepReductionForEveryBinding :: PrintingStyle -> [CoreBind] -> IO ()
printCoreStepByStepReductionForEveryBinding printingStyle bindings = do
  let allBindings = convertToBindingsList bindings
  mapM_ (printCoreStepByStepReductionForBinding printingStyle allBindings) allBindings

-- |takes a single core binding and shows a step-by-step reduction of the expression
printCoreStepByStepReductionForBinding :: PrintingStyle -> [Binding] -> Binding -> IO ()
printCoreStepByStepReductionForBinding printingStyle bindings (var, exp) = do
  putStr "\n**Reduction of "
  putStr (varToString var)
  putStr "**"
  putStrLn ""
  prettyPrint printingStyle exp
  printCoreStepByStepReductionForSingleExpression printingStyle bindings exp

-- |takes a core expression and shows a step-by-step reduction
printCoreStepByStepReductionForSingleExpression :: PrintingStyle -> [Binding] -> CoreExpr -> IO ()
printCoreStepByStepReductionForSingleExpression printingStyle bindings expression = do
  let (stepResults, reductionSuccessfulFlag) = getAllSteps bindings expression

  let stepResultsExceptLast = if null stepResults
                                then []
                                else init stepResults
  let stepResultsLast = ([last stepResults | not (null stepResults)])

  let filteredStepResults = (filter (\(stepDescription, _, _) -> not (shouldFilterOutReductionStep printingStyle stepDescription)) stepResultsExceptLast) ++ stepResultsLast 
  printStepResultList 0 printingStyle filteredStepResults reductionSuccessfulFlag

printStepResultList :: Int -> PrintingStyle -> [StepResult] -> ReductionSuccessfulFlag -> IO ()
printStepResultList _ _ [] True = putStrLn "{- reduction completed successfully -}"
printStepResultList _ _ [] False = putStrLn "{- reduction completed: no reduction rule implemented for this expression -}"
printStepResultList amountOfSkippedSteps printingStyle ((stepDescription, expression, _):xs) successFlat = do
  
  
  
  if shouldShowReductionStep stepDescription || null xs
    then do
      if amountOfSkippedSteps > 0
        then putStrLn ("{- skipping " ++ show amountOfSkippedSteps ++ " substeps -}")
        else putStr ""
      putStrLn ("{- " ++ show stepDescription ++ " -}")
      prettyPrint printingStyle expression
      printStepResultList 0 printingStyle xs successFlat
    else
      printStepResultList (amountOfSkippedSteps + 1) printingStyle xs successFlat

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


shouldFilterOutReductionStep :: PrintingStyle -> ReductionStepDescription -> Bool
shouldFilterOutReductionStep HaskellStyle (ApplicationStep argument) = isTypeInformation argument --do not show application of type variables, those informations are not shown when pretty printing haskell-like code
shouldFilterOutReductionStep HaskellStyle (EvaluationStep function) = isPrimitiveTypeConstructorName (varName function)
shouldFilterOutReductionStep style (NestedReduction nestedReduction) = shouldFilterOutReductionStep style (last nestedReduction)
shouldFilterOutReductionStep _ _ = False

shouldShowReductionStep :: ReductionStepDescription -> Bool
shouldShowReductionStep (EvaluationStep _) = True
shouldShowReductionStep PatternMatchStep = True
shouldShowReductionStep ConstructorArgumentReductionForVisualization = True
shouldShowReductionStep (NestedReduction reductions) = False
shouldShowReductionStep _ = False
