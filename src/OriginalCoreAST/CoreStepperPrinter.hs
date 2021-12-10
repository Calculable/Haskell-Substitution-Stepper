module OriginalCoreAST.CoreStepperPrinter
  (printCoreStepByStepReductionForBinding, printCoreStepByStepReductionForEveryBinding, convertToBindingsList, printCoreStepByStepReductionForSingleExpression
  )
where

import OriginalCoreAST.CoreStepperHelpers.CoreTransformator(convertToMultiArgumentFunction, convertFunctionApplicationWithArgumentListToNestedFunctionApplication)

import OriginalCoreAST.CoreInformationExtractorFunctions(varToString, canBeReduced)
import OriginalCoreAST.CoreStepper(applyStep, reduceToHeadNormalForm)
import Data.Maybe
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString)

import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import OriginalCoreAST.CorePrettyPrinter(prettyPrint)

type ReductionStepDescription = String --for example: "replace x with definition"
type Binding = (Var, Expr Var)

printCoreStepByStepReductionForEveryBinding :: [CoreBind] -> IO()
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

printCoreStepByStepReductionForSingleExpression :: [Binding] -> Expr Var -> IO(Expr Var)
printCoreStepByStepReductionForSingleExpression bindings expression     | canBeReduced expression = do
                                                                            let reduction = (applyStep bindings expression)
                                                                            case reduction of
                                                                                Just (reductionStepDescription, reducedExpression) -> do
                                                                                    putStrLn ("\n{-" ++ reductionStepDescription ++ "-}")
                                                                                    prettyPrint reducedExpression
                                                                                    printCoreStepByStepReductionForSingleExpression bindings reducedExpression
                                                                                Nothing -> do
                                                                                    putStrLn "\n{-no reduction rule implemented for this expression-}"
                                                                                    return expression
                                                                        | otherwise = do
                                                                            putStrLn "\n{-reduction complete (Head Normal Form)-}"
                                                                            --check if it can be reduced even more to wnormal form
                                                                            case expression of {
                                                                                (App expr argument) -> (do
                                                                                    let (function, arguments) = (convertToMultiArgumentFunction expression)
                                                                                    if (any canBeReduced arguments) 
                                                                                        then do
                                                                                            putStrLn "\n{-reduce to normal form-}"
                                                                                            let result = (convertFunctionApplicationWithArgumentListToNestedFunctionApplication function (map (reduceToHeadNormalForm bindings) arguments))
                                                                                            prettyPrint result
                                                                                            putStrLn "\n{-reduction complete (Normal Form)-}"
                                                                                            return result
                                                                                        else return expression);
                                                                                _ -> return expression
                                                                            }
                                                                            


convertToBindingsList :: [CoreBind] -> [Binding]
convertToBindingsList bindings = concatMap convertCoreBindingToBindingList bindings

convertCoreBindingToBindingList :: CoreBind -> [Binding]
convertCoreBindingToBindingList (NonRec binding exp) = [(binding, exp)]
convertCoreBindingToBindingList (Rec bindings) = bindings