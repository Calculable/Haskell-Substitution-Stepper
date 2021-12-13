module OriginalCoreAST.CoreStepperPrinter
  ( printCoreStepByStepReductionForBinding,
    printCoreStepByStepReductionForEveryBinding,
    convertToBindingsList,
    printCoreStepByStepReductionForSingleExpression,
  )
where

import Data.Maybe (fromJust, isNothing)
import GHC.Plugins (Bind (NonRec, Rec), CoreBind, Expr, Var)
import OriginalCoreAST.CoreInformationExtractorFunctions
  ( canBeReduced,
    varToString,
  )
import OriginalCoreAST.CorePrettyPrinter (prettyPrint)
import OriginalCoreAST.CoreStepper
  ( applyStep,
    canBeReducedToNormalForm,
    safeReduceToNormalForm,
  )
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator
  ( convertFunctionApplicationWithArgumentListToNestedFunctionApplication,
    convertToMultiArgumentFunction,
  )

type ReductionStepDescription = String --for example: "replace x with definition"

type Binding = (Var, Expr Var)

printCoreStepByStepReductionForEveryBinding :: [CoreBind] -> IO ()
printCoreStepByStepReductionForEveryBinding bindings = do
  let allBindings = convertToBindingsList bindings
  mapM_ (printCoreStepByStepReductionForBinding allBindings) allBindings

printCoreStepByStepReductionForBinding :: [Binding] -> Binding -> IO (Expr Var)
printCoreStepByStepReductionForBinding bindings (var, exp) = do
  putStr "\n**Reduction of "
  putStr (varToString var)
  putStr "**"
  putStrLn ""
  prettyPrint exp
  printCoreStepByStepReductionForSingleExpression bindings exp

printCoreStepByStepReductionForSingleExpression :: [Binding] -> Expr Var -> IO (Expr Var)
printCoreStepByStepReductionForSingleExpression bindings expression
  | canBeReduced expression = do
    let reduction = applyStep bindings expression
    case reduction of
      Just (reductionStepDescription, reducedExpression, newBindings) -> do
        putStrLn ("\n{-" ++ reductionStepDescription ++ "-}")
        prettyPrint reducedExpression
        printCoreStepByStepReductionForSingleExpression newBindings reducedExpression
      Nothing -> do
        putStrLn "\n{-no reduction rule implemented for this expression-}"
        return expression
  | otherwise = do
    --check if it can be reduced even more to normal form
    if canBeReducedToNormalForm expression
      then do
        putStrLn "\n{-reduction complete in Head Normal Form. I will try to reduce normal Form-}"
        let (function, arguments) = convertToMultiArgumentFunction expression
        let maybeArgumentsInNormalForm = map (safeReduceToNormalForm bindings) arguments
        if any isNothing maybeArgumentsInNormalForm
          then do
            putStrLn "\n{reduction to normal form not possible (most likely because it would result in an infinite-loop if recursive data structures are used)}"
            return expression
          else do
            let argumentsInNormalForm = map fromJust maybeArgumentsInNormalForm
            let result = convertFunctionApplicationWithArgumentListToNestedFunctionApplication function argumentsInNormalForm
            prettyPrint result
            putStrLn "\n{-reduction complete (Normal Form)-}"
            return result
      else do
        putStrLn "\n{-reduction complete (Normal Form)-}"
        return expression

convertToBindingsList :: [CoreBind] -> [Binding]
convertToBindingsList = concatMap convertCoreBindingToBindingList

convertCoreBindingToBindingList :: CoreBind -> [Binding]
convertCoreBindingToBindingList (NonRec binding exp) = [(binding, exp)]
convertCoreBindingToBindingList (Rec bindings) = bindings