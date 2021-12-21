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
printCoreStepByStepReductionForEveryBinding :: [CoreBind] -> IO ()
printCoreStepByStepReductionForEveryBinding bindings = do
  let allBindings = convertToBindingsList bindings
  mapM_ (printCoreStepByStepReductionForBinding allBindings) allBindings

-- |takes a single core binding and shows a step-by-step reduction of the expression
printCoreStepByStepReductionForBinding :: [Binding] -> Binding -> IO ()
printCoreStepByStepReductionForBinding bindings (var, exp) = do
  putStr "\n**Reduction of "
  putStr (varToString var)
  putStr "**"
  putStrLn ""
  prettyPrint exp
  printCoreStepByStepReductionForSingleExpression bindings exp

-- |takes a core expression and shows a step-by-step reduction
printCoreStepByStepReductionForSingleExpression :: [Binding] -> CoreExpr -> IO ()
printCoreStepByStepReductionForSingleExpression bindings expression = do
  let (stepResults, reductionSuccessfulFlag) = getAllSteps bindings expression
  let filteredStepResults = filter (\(stepDescription, _, _) -> shouldShowReductionStep stepDescription) stepResults
  printStepResultList (filteredStepResults, reductionSuccessfulFlag)

printStepResultList :: ([StepResult], ReductionSuccessfulFlag) -> IO ()
printStepResultList ([], True) = putStrLn "{- reduction completed successfully -}"
printStepResultList ([], False) = putStrLn "{- reduction completed: no reduction rule implemented for this expression -}"
printStepResultList ((stepDescription, expression, _):xs, successFlat) = do
  putStrLn ("{- " ++ show stepDescription ++ " -}")
  prettyPrint expression
  printStepResultList (xs, successFlat)

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


shouldShowReductionStep :: ReductionStepDescription -> Bool
shouldShowReductionStep (ApplicationStep argument) = not (isTypeInformation argument) --do not show application of type variables, those informations are not shown when pretty printing haskell-like code
shouldShowReductionStep (EvaluationStep function) = not (isPrimitiveTypeConstructorName (varName function))
shouldShowReductionStep _ = True