module OriginalCoreAST.CoreStepperPrinter
  (printCoreStepByStepReduction, printCoreStepByStepReductionForEveryBinding
  )
where

import OriginalCoreAST.CoreInformationExtractorFunctions(varToString, canBeReduced)
import OriginalCoreAST.CoreStepper(applyStep)
import Data.Maybe
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )

import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import OriginalCoreAST.CorePrettyPrinter(prettyPrint)

type ReductionStepDescription = String --for example: "replace x with definition"
type Binding = (Var, Expr Var)


printCoreStepByStepReductionForEveryBinding :: [CoreBind] -> IO()
printCoreStepByStepReductionForEveryBinding bindings = do
    let allBindings = convertToBindingsList bindings
    mapM_ (printCoreStepByStepReduction allBindings) allBindings

printCoreStepByStepReduction :: [Binding] -> Binding -> IO ()
printCoreStepByStepReduction bindings (var, exp) = do
    putStr "\n**Reduction of "
    putStr (varToString var)
    putStr "**"
    putStrLn ""
    prettyPrint exp
    reduce bindings exp

convertToBindingsList :: [CoreBind] -> [Binding]
convertToBindingsList bindings = concat (map convertCoreBindingToBindingList bindings)

convertCoreBindingToBindingList :: CoreBind -> [Binding]
convertCoreBindingToBindingList (NonRec binding exp) = [(binding, exp)]
convertCoreBindingToBindingList (Rec bindings) = bindings


reduce :: [Binding] -> Expr Var -> IO()
reduce bindings expression    | canBeReduced expression = do
                                let reduction = (applyStep bindings expression)
                                case reduction of
                                    Just (reductionStepDescription, reducedExpression) -> do
                                        putStrLn ("\n{-" ++ reductionStepDescription ++ "-}")
                                        prettyPrint reducedExpression
                                        reduce bindings reducedExpression
                                    Nothing -> putStrLn "\n{-no reduction rule implemented for this expression-}"
                              | otherwise = putStrLn "\n{-reduction complete-}"


