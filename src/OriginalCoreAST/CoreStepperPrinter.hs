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
printCoreStepByStepReductionForBinding :: [Binding] -> Binding -> IO CoreExpr
printCoreStepByStepReductionForBinding bindings (var, exp) = do
  putStr "\n**Reduction of "
  putStr (varToString var)
  putStr "**"
  putStrLn ""
  prettyPrint exp
  printCoreStepByStepReductionForSingleExpression bindings exp

-- |takes a core expression and shows a step-by-step reduction
printCoreStepByStepReductionForSingleExpression :: [Binding] -> CoreExpr -> IO CoreExpr
printCoreStepByStepReductionForSingleExpression bindings expression
  | canBeReduced expression = do
    let reduction = applyStep bindings expression
    case reduction of
      Just (reductionStepDescription, reducedExpression, newBindings) -> do
        if shouldShowReductionStep reductionStepDescription
          then do          
            putStrLn ("\n{-" ++ show reductionStepDescription ++ "-}")
            prettyPrint reducedExpression
            printCoreStepByStepReductionForSingleExpression newBindings reducedExpression
          else printCoreStepByStepReductionForSingleExpression newBindings reducedExpression
      Nothing -> do
        putStrLn "\n{-no reduction rule implemented for this expression-}"
        return expression
  | otherwise = do
    --check if it can be reduced even more to normal form
    if canBeReducedToNormalForm expression
      then do
        let (function, arguments) = convertToMultiArgumentFunction expression
        let maybeArgumentsInNormalForm = map (safeReduceToNormalForm bindings) arguments
        if any isNothing maybeArgumentsInNormalForm
          then do
            return expression
          else do
            let argumentsInNormalForm = map fromJust maybeArgumentsInNormalForm
            let result = convertFunctionApplicationWithArgumentListToNestedFunctionApplication function argumentsInNormalForm
            putStrLn "\n{-reduction complete - reduce constructor arguments for better visualization-}"
            prettyPrint result
            putStrLn "\n{-reduction complete-}"
            return result
      else do
        putStrLn "\n{-reduction complete-}"
        return expression

-- |converts a list of (nested/recursive) CoreBinds into one single flat list of bindings
convertToBindingsList :: [CoreBind] -> [Binding]
convertToBindingsList = concatMap convertCoreBindingToBindingList
  where
    convertCoreBindingToBindingList :: CoreBind -> [Binding]
    convertCoreBindingToBindingList (NonRec binding exp) = [(binding, exp)]
    convertCoreBindingToBindingList (Rec bindings) = bindings


shouldShowReductionStep :: ReductionStepDescription -> Bool
shouldShowReductionStep (EvaluationStep _) = True
shouldShowReductionStep PatternMatchStep = True
shouldShowReductionStep StrictApplicationArgumentStep = True
shouldShowReductionStep _ = False
