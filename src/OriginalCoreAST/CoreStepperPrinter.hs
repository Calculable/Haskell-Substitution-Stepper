module OriginalCoreAST.CoreStepperPrinter
  ( printCoreStepByStepReductionForBinding,
    printCoreStepByStepReductionForEveryBinding,
  )
where

import GHC.Plugins (Bind (NonRec, Rec), CoreBind, Expr, Var)
import OriginalCoreAST.CoreInformationExtractorFunctions
  ( canBeReduced,
    varToString,
  )
import OriginalCoreAST.CorePrettyPrinter (prettyPrint)
import OriginalCoreAST.CoreStepper (applyStep)

type ReductionStepDescription = String --for example: "replace x with definition"

type Binding = (Var, Expr Var)

printCoreStepByStepReductionForEveryBinding :: [CoreBind] -> IO ()
printCoreStepByStepReductionForEveryBinding bindings = do
  let allBindings = convertToBindingsList bindings
  mapM_ (printCoreStepByStepReductionForBinding allBindings) allBindings

printCoreStepByStepReductionForBinding :: [Binding] -> Binding -> IO ()
printCoreStepByStepReductionForBinding bindings (var, exp) = do
  putStr "\n**Reduction of "
  putStr (varToString var)
  putStr "**"
  putStrLn ""
  prettyPrint exp
  printCoreStepByStepReductionForSingleExpression bindings exp

printCoreStepByStepReductionForSingleExpression :: [Binding] -> Expr Var -> IO ()
printCoreStepByStepReductionForSingleExpression bindings expression
  | canBeReduced expression = do
    let reduction = applyStep bindings expression
    case reduction of
      Just (reductionStepDescription, reducedExpression) -> do
        putStrLn ("\n{-" ++ reductionStepDescription ++ "-}")
        prettyPrint reducedExpression
        printCoreStepByStepReductionForSingleExpression bindings reducedExpression
      Nothing -> putStrLn "\n{-no reduction rule implemented for this expression-}"
  | otherwise = putStrLn "\n{-reduction complete-}"

convertToBindingsList :: [CoreBind] -> [Binding]
convertToBindingsList = concatMap convertCoreBindingToBindingList

convertCoreBindingToBindingList :: CoreBind -> [Binding]
convertCoreBindingToBindingList (NonRec binding exp) = [(binding, exp)]
convertCoreBindingToBindingList (Rec bindings) = bindings