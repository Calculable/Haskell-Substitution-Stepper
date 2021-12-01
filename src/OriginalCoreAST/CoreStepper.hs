module OriginalCoreAST.CoreStepper(applyStep)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.List(isPrefixOf, find)
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
import Data.List(isPrefixOf)
import GHC.Types.Name(nameUnique, Name, mkSystemVarName, mkSysTvName, mkSystemName, pprNameUnqualified, nameStableString, getOccString)
import GHC.Types.Unique (minLocalUnique)
import GHC.Data.FastString (mkFastString)
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))
import GHC.Types.Id.Info ( vanillaIdInfo, IdDetails(..))
import GHC.Types.Name.Occurrence (mkOccName, mkVarOcc)
import OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToExpression)
import OriginalCoreAST.CoreInformationExtractorFunctions(showVarExpression, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced)

type ReductionStepDescription = String --for example: "replace x with definition"
type Binding = (Var, Expr Var)





applyStep :: [Binding] -> Expr Var -> Maybe (ReductionStepDescription, Expr Var)
applyStep bindings (Var name) = do
    foundBinding <- tryFindBinding name bindings
    return (("Replace '" ++ (varToString name) ++ "' with definition"),foundBinding) {-replace binding reference with actual expression (Delta Reduction)-}                                
applyStep bindings (App (Lam parameter expression) argument) = do
    Just ("Lamda Application", deepReplaceVarWithinExpression parameter argument expression)
applyStep bindings (App (App first second) third) = do
    simplifyNestedApp bindings (App (App first second) third) --nested app
applyStep bindings (App (Var name) argument) = do
    let expression = tryFindBinding name bindings
    if isNothing expression 
        then do
            simplifyNestedApp bindings (App (Var name) argument)
        else Just ("Replace '" ++ (varToString name) ++ "' with definition", (App (fromJust expression) argument))
applyStep bindings (Case expression binding caseType alternatives) = do
    if (canBeReduced expression) 
        then do 
            (description, reducedExpression) <- applyStep bindings expression
            return (description, (Case reducedExpression binding caseType alternatives))
        else do
            matchingPattern <- findMatchingPattern expression alternatives
            return ("Replace with matching pattern", matchingPattern)
applyStep _ _ = do
    Nothing

simplifyNestedApp :: [Binding] -> Expr Var -> Maybe (ReductionStepDescription, Expr Var) --eval if all parameters are reduced
simplifyNestedApp bindings expr = do
    let (function, arguments) = (convertToMultiArgumentFunction expr)
    case function of
        (Var var) -> if (isNothing (tryFindBinding var bindings))
                        then if (any canBeReduced arguments) 
                            then do 
                                (description, simplifiedArguments) <- (applyStepToOneOfTheArguments bindings [] arguments)
                                return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication function simplifiedArguments)
                            else do
                                appliedFunction <- applyFunctionToArguments function arguments --all arguments are reduced, eval function. This is stric behaviour! We have to use strict behaviour here because we are trying to evaluate a function whose definition we do not know. therefor we cannot apply the arguments one after another but have to simplify all arguments before calling the function 
                                return ("Apply " ++ (showOutputable function), appliedFunction) 
                        else do
                            (description, reducedFunction) <- applyStep bindings (App function (head arguments))
                            return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments))                            
        (Lam _ _) -> do
            (description, reducedFunction) <- applyStep bindings (App function (head arguments))
            return (description, convertFunctionApplicationWithArgumentListToNestedFunctionApplication reducedFunction (tail arguments))

applyStepToOneOfTheArguments :: [Binding] -> [Expr Var] -> [Expr Var] -> Maybe (ReductionStepDescription, [Expr Var])
applyStepToOneOfTheArguments bindings alreadyReducedArguments (x:xs) = if canBeReduced x 
                                                                        then do
                                                                            (description, reducedArgument) <- applyStep bindings x
                                                                            return (description, (alreadyReducedArguments ++ [reducedArgument]) ++ xs)
                                                                        else applyStepToOneOfTheArguments bindings (alreadyReducedArguments ++ [x]) xs
applyStepToOneOfTheArguments bindings alreadyReducedArguments [] = Nothing --no argument that can be reduced was found. this should not happen because this condition gets checked earlier in the code

convertFunctionApplicationWithArgumentListToNestedFunctionApplication :: Expr Var -> [Expr Var] -> Expr Var
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression [] = expression
convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression arguments = App (convertFunctionApplicationWithArgumentListToNestedFunctionApplication expression (init arguments)) (last arguments)

applyFunctionToArguments :: Expr Var -> [Expr Var] -> Maybe (Expr Var) 
applyFunctionToArguments (Var functionOrOperatorName) arguments = do
    applyUnsteppableFunctionToArguments (varToString functionOrOperatorName) (filter (not.isTypeInformation) arguments) --Precondition: function must be in the form of "var" and all arguments must be in the form of. This is already checked by the function which is calling this function
applyFunctionToArguments _ _ = error "function-expression has to be a 'Var'"

applyUnsteppableFunctionToArguments :: String -> [Expr Var] -> Maybe (Expr Var) 
applyUnsteppableFunctionToArguments "+" [x, y] = Just ((+) x y)
applyUnsteppableFunctionToArguments "-" [x, y] = Just ((-) x y)
applyUnsteppableFunctionToArguments "*" [x, y] = Just ((*) x y)
applyUnsteppableFunctionToArguments "/" [x, y] = Just ((/) x y)
applyUnsteppableFunctionToArguments "recip" [x] = Just (recip x)
applyUnsteppableFunctionToArguments "signum" [x] = Just (signum x)
applyUnsteppableFunctionToArguments "abs" [x] = Just (abs x)
applyUnsteppableFunctionToArguments "/=" [x, y] = Just (boolToExpression ((/=) x y))
applyUnsteppableFunctionToArguments "==" [x, y] = Just (boolToExpression ((==) x y))
applyUnsteppableFunctionToArguments "<" [x, y] = Just (boolToExpression ((<) x y))
applyUnsteppableFunctionToArguments ">" [x, y] = Just (boolToExpression ((>) x y))
applyUnsteppableFunctionToArguments ">=" [x, y] = Just (boolToExpression ((>=) x y))
applyUnsteppableFunctionToArguments "<=" [x, y] = Just (boolToExpression ((<=) x y))
applyUnsteppableFunctionToArguments "negate" [(Lit (LitNumber _ x))] = Just (integerToCoreExpression (negate x)) --example of an arbitrary function from the prelude. note how the arguments must have the right type and the result is converted back into an expressino
applyUnsteppableFunctionToArguments "unpackCString#" [x] = Just x
applyUnsteppableFunctionToArguments name _ = Nothing --function not supported
-- toDo: Add more functions from the prelude

--boolToExpression :: Bool -> Expr Var  --this is a wild hack, i just took the easiest constructors i found to create a "Var"-Instance without understanding what those constructors stand form 
--boolToExpression True = Var (mkCoVar (mkSystemVarName minLocalUnique (mkFastString "True")) (LitTy (StrTyLit (mkFastString "Bool"))))
--boolToExpression False = Var (mkCoVar (mkSystemVarName minLocalUnique (mkFastString "False")) (LitTy (StrTyLit (mkFastString "Bool"))))




tryFindBinding :: Var -> [Binding] -> Maybe (Expr Var)
tryFindBinding key [] = Nothing
tryFindBinding key ((var, exp):xs) = if ((==) (varToString var) (varToString key))
                                                    then Just (exp)
                                                    else tryFindBinding key xs

deepReplaceVarWithinExpression :: Var -> Expr Var -> Expr Var -> Expr Var
deepReplaceVarWithinExpression name replaceExpression (Var varName) = if (==) (varToString varName) (varToString name) then replaceExpression else (Var varName)
deepReplaceVarWithinExpression name replaceExpression (App expression argument) = App (deepReplaceVarWithinExpression name replaceExpression expression) (deepReplaceVarWithinExpression name replaceExpression argument)
deepReplaceVarWithinExpression name replaceExpression (Lam parameter expression) =
  if (varToString parameter) == (varToString name)
    then Lam parameter expression --do nothing, use local lamda parameter with the same name (shadowing)
    else Lam parameter (deepReplaceVarWithinExpression name replaceExpression expression)
deepReplaceVarWithinExpression name replaceExpression (Case expression binding caseType alternatives) = Case (deepReplaceVarWithinExpression name replaceExpression expression) binding caseType (map (deepReplaceVarWithinAlternative name replaceExpression) alternatives)
deepReplaceVarWithinExpression _ _ expression = expression --nothing to replace (ToDo: Let, Cast, Tick, Type, Coercion not implemented yet)

deepReplaceVarWithinAlternative :: Var -> Expr Var -> Alt Var -> Alt Var 
deepReplaceVarWithinAlternative name replaceExpression (altCon, localBoundVars, expression) = if (elem (varToString name) (map varToString localBoundVars))
                                                                                                 then (altCon, localBoundVars, expression) --do nothing, use local parameter with the same name (shadowing)
                                                                                                 else (altCon, localBoundVars, (deepReplaceVarWithinExpression name replaceExpression expression))

deepReplaceMultipleVarWithinExpression :: [Var] -> [Expr Var] -> Expr Var -> Expr Var
deepReplaceMultipleVarWithinExpression [] _ expression = expression
deepReplaceMultipleVarWithinExpression _ [] expression = expression
deepReplaceMultipleVarWithinExpression (x:xs) (y:ys) expression = deepReplaceMultipleVarWithinExpression xs ys (deepReplaceVarWithinExpression x y expression)

findMatchingPattern :: Expr Var -> [Alt Var] -> Maybe (Expr Var)
findMatchingPattern expression [] = Nothing 
findMatchingPattern _ ((DEFAULT, _, expression):_) = Just expression
findMatchingPattern (Var name) (((DataAlt dataCon), _, expression):xs) = if ((==) (varToString name) (showOutputable dataCon)) --check: is there a more elegant way than "show outputable"
                                                                                then Just expression
                                                                                else (findMatchingPattern (Var name) xs)
findMatchingPattern (Lit literal) (((LitAlt patternLiteral), _, expression):xs) = if ((==) literal patternLiteral) --can we compare two literals like this?
                                                                                             then Just expression
                                                                                             else (findMatchingPattern (Lit literal) xs)
findMatchingPattern (App expr argument) (((DataAlt patternConstructorName), boundNames, expression):xs) = do
    let (function, arguments) = convertToMultiArgumentFunction (App expr argument)
    if ((==) (showVarExpression function) (showOutputable patternConstructorName)) --check: is there a more elegant way than "show outputable"
        then Just (deepReplaceMultipleVarWithinExpression boundNames (filter (not.isTypeInformation) arguments) expression)
        else (findMatchingPattern (App expr argument) xs) 
findMatchingPattern expression (x:xs) = findMatchingPattern expression xs


--core utilities 

convertToMultiArgumentFunction :: Expr Var -> (Expr Var, [Expr Var])
convertToMultiArgumentFunction expr = collectArgs expr