module OriginalCoreAST.CoreStepper(applyStep, reduceToHeadNormalForm, reduceToNormalForm, canBeReducedToNormalForm, safeReduceToNormalForm)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.List ( isPrefixOf, find, isPrefixOf )
import SimplifiedCoreAST.SimplifiedCoreASTPrinter (printSimplifiedCoreExpression)
import Data.Maybe
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import GHC.Utils.Outputable (Outputable (ppr), OutputableBndr, showSDoc, showSDocUnsafe)
import GHC.Core.Utils (exprIsHNF)
import Utils (showOutputable)
import GHC.Core.Ppr
  ( pprCoreAlt,
  )
import GHC.Types.Name(nameUnique, Name, mkSystemVarName, mkSysTvName, mkSystemName, pprNameUnqualified, nameStableString, getOccString, dataName)
import GHC.Types.Unique (minLocalUnique)
import GHC.Data.FastString (mkFastString)
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))
import GHC.Types.Id.Info ( vanillaIdInfo, IdDetails(..))
import GHC.Types.Name.Occurrence (mkOccName, mkVarOcc)
import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced, isVarExpression, isClassDictionary)
import OriginalCoreAST.CoreStepperHelpers.CoreEvaluator(evaluateFunctionWithArguments)
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator(convertFunctionApplicationWithArgumentListToNestedFunctionApplication, deepReplaceVarWithinExpression, deepReplaceVarWithinAlternative, deepReplaceMultipleVarWithinExpression, convertToMultiArgumentFunction)
import OriginalCoreAST.CoreStepperHelpers.CoreLookup(tryFindBinding, findMatchingPattern)
import Debug.Trace(trace)

type ReductionStepDescription = String --for example: "replace x with definition"
type Binding = (Var, Expr Var)
type StepResult = (ReductionStepDescription, Expr Var, [Binding])

reduceToNormalForm :: [Binding] -> Expr Var -> Expr Var
reduceToNormalForm bindings expression = do
    fromJust (reduceToNormalFormWithMaximumAmountOfReductions (negate 1) bindings expression)

maximumAmoutOfReductionsBeforeError = 99

safeReduceToNormalForm :: [Binding] -> Expr Var -> Maybe (Expr Var)
safeReduceToNormalForm bindings expression = reduceToNormalFormWithMaximumAmountOfReductions maximumAmoutOfReductionsBeforeError bindings expression

reduceToNormalFormWithMaximumAmountOfReductions :: Integer -> [Binding] -> Expr Var -> Maybe (Expr Var)
reduceToNormalFormWithMaximumAmountOfReductions 0 _ _ = trace "infinite loop" Nothing
reduceToNormalFormWithMaximumAmountOfReductions maximumAmoutOfReductions bindings expression = do
    expressionInHeadNormalForm <- reduceToHeadNormalForm bindings expression
    if canBeReducedToNormalForm expressionInHeadNormalForm
        then do
            let (function, arguments) = convertToMultiArgumentFunction expressionInHeadNormalForm
            let maybeReducedArguments = map (reduceToNormalFormWithMaximumAmountOfReductions (maximumAmoutOfReductions-1) bindings) arguments
            if (any isNothing maybeReducedArguments)
                then Nothing
                else (Just $ convertFunctionApplicationWithArgumentListToNestedFunctionApplication function (map fromJust maybeReducedArguments))

            
        else (Just expressionInHeadNormalForm)


reduceToHeadNormalForm :: [Binding] -> Expr Var -> Maybe (Expr Var)
reduceToHeadNormalForm bindings expression  | canBeReduced expression = do
                                                let reduction = applyStep bindings expression
                                                case reduction of
                                                    Just (reductionStepDescription, reducedExpression, newBindings) -> reduceToHeadNormalForm newBindings reducedExpression
                                                    Nothing -> trace ("Debug - Here is the expression for which no reduction rule is implemented: " ++ showOutputable expression) Nothing
                                            | otherwise = Just expression

applyStep :: [Binding] -> Expr Var -> Maybe StepResult
applyStep bindings (Var name) = do
    foundBinding <- tryFindBinding name bindings
    return (("Replace '" ++ (varToString name) ++ "' with definition"),foundBinding, bindings) {-replace binding reference with actual expression (Delta Reduction)-}
applyStep bindings (App (Lam parameter expression) argument) = do
    Just ("Lamda Application", deepReplaceVarWithinExpression parameter argument expression, bindings)
applyStep bindings (App (Let binding expression) argument) = do
    (description, reducedLet, newBindings) <- applyStep bindings (Let binding expression) 
    return (description, App reducedLet argument, newBindings)
applyStep bindings (App (App first second) third) = do
    (applyStepToNestedApp bindings (App (App first second) third)) --nested app
applyStep bindings (App (Var name) argument) = do
    let expression = tryFindBinding name bindings
    if isNothing expression
        then do
            (applyStepToNestedApp bindings (App (Var name) argument))
        else Just ("Replace '" ++ (varToString name) ++ "' with definition", (App (fromJust expression) argument), bindings)
applyStep bindings (Case expression binding caseType alternatives) = do
    if (canBeReduced expression)
        then do
            (description, reducedExpression, newBindings) <- applyStep bindings expression
            return (description, (Case reducedExpression binding caseType alternatives), newBindings)
        else do
            matchingPattern <- findMatchingPattern expression alternatives
            return ("Replace with matching pattern", matchingPattern, bindings)
applyStep bindings (Let (NonRec b expr) expression) = Just ("Replace '" ++ varToString b ++ "' with definition", deepReplaceVarWithinExpression b expr expression, bindings)
applyStep bindings (Let (Rec [(b, expr)]) expression) = Just ("Replace '" ++ varToString b ++ "' with definition", deepReplaceVarWithinExpression b expr expression, ((b, expr) : bindings))

applyStep bindings (Cast _ _) = trace "no applicable step found: cast is not yet supported" Nothing
applyStep bindings (Tick _ _) = trace "no applicable step found: tick is not supported" Nothing
applyStep bindings (Coercion _) = trace "no applicable step found: coercion is not yet supported" Nothing
applyStep _ _ = trace "no applicable step found" Nothing

applyStepToNestedApp :: [Binding] -> Expr Var -> Maybe StepResult
applyStepToNestedApp bindings expr = do
    let maybeResult = tryApplyStepToApplicationUsingClassDictionary bindings expr
    if isJust maybeResult
        then maybeResult
        else do
            let (function, arguments) = (convertToMultiArgumentFunction expr)
            case function of
                (Var var) -> if (isNothing (tryFindBinding var bindings))
                                then (if (any canBeReduced arguments)
                                    then do
                                        (description, simplifiedArguments, newBindings) <- (applyStepToOneOfTheArguments bindings [] arguments)
                                        return (description, (convertFunctionApplicationWithArgumentListToNestedFunctionApplication function simplifiedArguments), newBindings)
                                    else do
                                        appliedFunction <- (evaluateFunctionWithArguments function arguments (reduceToHeadNormalForm bindings)) --all arguments are reduced, eval function. This is stric behaviour! We have to use strict behaviour here because we are trying to evaluate a function whose definition we do not know. therefor we cannot apply the arguments one after another but have to simplify all arguments before calling the function 
                                        return ("Apply " ++ (showOutputable function), appliedFunction, bindings))
                                else do
                                    (description, reducedFunction, newBindings) <- applyStep bindings (App function (head arguments))
                                    return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments), newBindings)
                (Lam _ _) -> do
                    (description, reducedFunction, newBindings) <- applyStep bindings (App function (head arguments))
                    return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments), newBindings)
                _ -> trace "application with unsupported expression type" Nothing

applyStepToOneOfTheArguments :: [Binding] -> [Expr Var] -> [Expr Var] -> Maybe (ReductionStepDescription, [Expr Var], [Binding])
applyStepToOneOfTheArguments bindings alreadyReducedArguments (x:xs) = if canBeReduced x
                                                                        then do
                                                                            (description, reducedArgument, newBindings) <- applyStep bindings x
                                                                            return (description, (alreadyReducedArguments ++ [reducedArgument]) ++ xs, newBindings)
                                                                        else applyStepToOneOfTheArguments bindings (alreadyReducedArguments ++ [x]) xs
applyStepToOneOfTheArguments bindings alreadyReducedArguments [] = error "no reducable argument found" --no argument that can be reduced was found. this should not happen because this condition gets checked earlier in the code


-- | The "canBeReducedFunction" checks if a Core expression is not yet in normal form and can further be reduced
canBeReducedToNormalForm :: Expr Var -> Bool
canBeReducedToNormalForm (App expr argument) = do
    let (function, arguments) = (convertToMultiArgumentFunction (App expr argument))
    (any canBeReduced arguments) || (any canBeReducedToNormalForm arguments)  
canBeReducedToNormalForm _ = False

tryApplyStepToApplicationUsingClassDictionary :: [Binding] -> Expr Var -> Maybe StepResult
tryApplyStepToApplicationUsingClassDictionary _ _ = Nothing 

functionCallContainsClassDictionary :: Expr Var -> Bool --example for a function call with class dictionary: == @Direction $fEqDirection Top Down
functionCallContainsClassDictionary expr = do
    let (function, arguments) = (convertToMultiArgumentFunction expr)
    if ((length arguments) >= 2)
        then do
            ((isVarExpression function) && (isTypeInformation (arguments!!0))) && (isClassDictionary (arguments!!1))
        else False 
