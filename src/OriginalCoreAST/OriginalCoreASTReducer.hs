module OriginalCoreAST.OriginalCoreASTReducer
  (printCoreStepByStepReduction, printCoreStepByStepReductionForEveryBinding
  )
where

import SimplifiedCoreAST.SimplifiedCoreAST (ExpressionS(..), LiteralS(..), AltS(..), AltConS(..), BindS(..))
import Data.List(isPrefixOf, find)
import SimplifiedCoreAST.SimplifiedCoreASTPrinter (printSimplifiedCoreExpression)
import Data.Maybe
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import GHC.Utils.Outputable (Outputable (ppr), OutputableBndr)
import GHC.Core.Utils (exprIsHNF)
import Utils (showOutputable)
import GHC.Core.Ppr
  ( pprCoreAlt,
  )
import Data.List(isPrefixOf)
import SimplifiedCoreAST.SimplifiedCoreAST (ExpressionS(..), LiteralS(..), AltS(..), AltConS(..), BindS(..))
import GHC.Types.Name(nameUnique, Name, mkSystemVarName, mkSysTvName, mkSystemName)
import GHC.Types.Unique (minLocalUnique)
import GHC.Data.FastString (mkFastString)
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))
import GHC.Types.Id.Info ( vanillaIdInfo, IdDetails(..))
import GHC.Types.Name.Occurrence (mkOccName, mkVarOcc)
type ReductionStepDescription = String --for example: "replace x with definition"
type Binding = (Var, Expr Var)

{-Type Classes-}

instance Num (Expr b) where
  (+) (Lit x) (Lit y) = Lit ((+) x y)
  (+) _ _ = error "+ not supported by this type"
  (-) (Lit x) (Lit y) = Lit ((-) x y)
  (-) _ _ = error "- not supported by this type"
  (*) (Lit x) (Lit y) = Lit ((*) x y)
  (*) _ _ = error "* not supported by this type"
  signum (Lit x) = Lit (signum x)
  signum _ = error "signum not supported by this type"
  fromInteger x = integerToCoreExpression x
  abs (Lit x) = Lit (abs x)
  abs _ = error "abs not supported by this type"

instance Fractional (Expr b) where
  (/) (Lit x) (Lit y) = Lit ((/) x y)
  (/) _ _ = error "/ not supported by this type"
  recip (Lit x) = Lit (recip x)
  recip _ = error "recip not supported by this type"
  fromRational x = rationalToCoreExpression x

instance Eq (Expr b) where
  (/=) (Lit x) (Lit y) = (/=) x y
  (/=) _ _ = error "/= not supported by this type"
  (==) (Lit x) (Lit y) = (==) x y
  (==) _ _ = error "== not supported by this type"

instance Ord (Expr b) where
  (<=) (Lit x) (Lit y) = (<=) x y
  (<=) _ _ = error "<= not supported by this type"
  (<) (Lit x) (Lit y) = (<=) x y
  (<) _ _ = error "< not supported by this type"
  (>=) (Lit x) (Lit y) = (<=) x y
  (>=) _ _ = error ">= not supported by this type"
  (>) (Lit x) (Lit y) = (<=) x y
  (>) _ _ = error "> not supported by this type"

instance Num Literal where
  (+) (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral ((Prelude.+) x y)
  (+) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.+) (fromInteger x) (fromRational y))
  (+) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromInteger y))
  (+) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.+) (fromInteger x) (fromRational y))
  (+) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromInteger y))
  (+) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.+) (fromRational x) (fromRational y))
  (+) _ _ = error "+ not supported by this type"
  (-) (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral ((Prelude.-) x y)
  (-) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.-) (fromInteger x) (fromRational y))
  (-) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromInteger y))
  (-) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.-) (fromInteger x) (fromRational y))
  (-) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromInteger y))
  (-) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.-) (fromRational x) (fromRational y))
  (-) _ _ = error "- not supported by this type"
  (*) (LitNumber _ x) (LitNumber _ y) = integerToCoreLiteral ((Prelude.*) x y)
  (*) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.*) (fromInteger x) (fromRational y))
  (*) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromInteger y))
  (*) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.*) (fromInteger x) (fromRational y))
  (*) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromInteger y))
  (*) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude.*) (fromRational x) (fromRational y))
  (*) _ _ = error "* not supported by this type"
  signum (LitNumber _ x) = integerToCoreLiteral (signum (fromInteger x))
  signum (LitDouble x) = fractionalToCoreLiteral (signum x)
  signum (LitFloat x) = fractionalToCoreLiteral (signum x)
  signum _ = error "signum not supported for this type"
  fromInteger x = integerToCoreLiteral x
  abs (LitNumber _ x) = integerToCoreLiteral (abs x)
  abs (LitDouble x) = fractionalToCoreLiteral (abs (fromRational x))
  abs (LitFloat x) = fractionalToCoreLiteral (abs (fromRational x))
  abs _ = error "abs not supported for this type"

instance Fractional Literal where
  (/) (LitNumber _ x) (LitNumber _ y) = LitDouble ((Prelude./) (fromInteger x) (fromInteger y))
  (/) (LitDouble x) (LitDouble y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) (LitNumber _ x) (LitDouble y) = fractionalToCoreLiteral ((Prelude./) (fromInteger x) y)
  (/) (LitDouble x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude./) x (fromInteger y))
  (/) (LitFloat x) (LitFloat y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) (LitNumber _ x) (LitFloat y) = fractionalToCoreLiteral ((Prelude./) (fromInteger x) y)
  (/) (LitFloat x) (LitNumber _ y) = fractionalToCoreLiteral ((Prelude./) x (fromInteger y))
  (/) (LitFloat x) (LitDouble y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) (LitDouble x) (LitFloat y) = fractionalToCoreLiteral ((Prelude./) x y)
  (/) _ _ = error "/ not supported by this type"
  recip expression = 1 / expression
  fromRational x = (LitDouble x)

-- instance EQ, instance Ord for Literal is already implemented inside GHC


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
    putStr (showOutputable exp)
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
                                        putStr (showOutputable reducedExpression)
                                        reduce bindings reducedExpression
                                    Nothing -> putStrLn "\n{-no reduction rule implemented for this expression-}"
                              | otherwise = putStrLn "\n{-reduction complete-}"


applyStep :: [Binding] -> Expr Var -> Maybe (ReductionStepDescription, Expr Var)
applyStep bindings (Var name) = do
    foundBinding <- tryFindBinding name bindings
    return (("Replace '" ++ (varToString name) ++ "' with definition"),foundBinding) {-replace binding reference with actual expression (Delta Reduction)-}                                
applyStep bindings (App (Lam parameter expression) argument) = Just ("Lamda Application", deepReplaceVarWithinExpression parameter argument expression)
applyStep bindings (App (App first second) third) = simplifyNestedApp bindings (App (App first second) third) --nested app
applyStep bindings (App (Var name) argument) = do
    let expression = tryFindBinding name bindings
    if isNothing expression 
        then simplifyNestedApp bindings (App (Var name) argument)
        else Just ("Replace '" ++ (varToString name) ++ "' with definition", (App (fromJust expression) argument))
applyStep bindings (Case expression binding caseType alternatives) = do
    (description, reducedExpression) <- applyStep bindings expression
    if (canBeReduced expression) 
        then return (description, (Case reducedExpression binding caseType alternatives))
        else do
            matchingPattern <- findMatchingPattern expression alternatives
            return ("Replace with matching pattern", matchingPattern)

applyStep _ _ = Nothing

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
applyFunctionToArguments (Var functionOrOperatorName) arguments = applyUnsteppableFunctionToArguments (varToString functionOrOperatorName) (filter (not.isTypeInformation) arguments) --Precondition: function must be in the form of "var" and all arguments must be in the form of. This is already checked by the function which is calling this function
applyFunctionToArguments _ _ = error "function-expression has to be a 'Var'"

applyUnsteppableFunctionToArguments :: String -> [Expr Var] -> Maybe (Expr Var) 
applyUnsteppableFunctionToArguments "+" [x, y] = Just ((+) x y)
applyUnsteppableFunctionToArguments "-" [x, y] = Just ((-) x y)
applyUnsteppableFunctionToArguments "*" [x, y] = Just ((*) x y)
applyUnsteppableFunctionToArguments "/" [x, y] = Just ((/) x y)
applyUnsteppableFunctionToArguments "recip" [x] = Just (signum x)
applyUnsteppableFunctionToArguments "signum" [x] = Just (signum x)
applyUnsteppableFunctionToArguments "abs" [x] = Just (abs x)
applyUnsteppableFunctionToArguments "/=" [x, y] = Just (boolToExpression ((/=) x y))
applyUnsteppableFunctionToArguments "==" [x, y] = Just (boolToExpression ((==) x y))
applyUnsteppableFunctionToArguments "<" [x, y] = Just (boolToExpression ((<) x y))
applyUnsteppableFunctionToArguments ">" [x, y] = Just (boolToExpression ((>) x y))
applyUnsteppableFunctionToArguments ">=" [x, y] = Just (boolToExpression ((>=) x y))
applyUnsteppableFunctionToArguments "<=" [x, y] = Just (boolToExpression ((<=) x y))
applyUnsteppableFunctionToArguments "negate" [(Lit (LitNumber _ x))] = Just (integerToCoreExpression (negate x)) --example of an arbitrary function from the prelude. note how the arguments must have the right type and the result is converted back into an expressino
applyUnsteppableFunctionToArguments name _ = Nothing --function not supported
-- toDo: Add more functions from the prelude

--boolToExpression :: Bool -> Expr Var  --this is a wild hack, i just took the easiest constructors i found to create a "Var"-Instance without understanding what those constructors stand form 
--boolToExpression True = Var (mkCoVar (mkSystemVarName minLocalUnique (mkFastString "True")) (LitTy (StrTyLit (mkFastString "Bool"))))
--boolToExpression False = Var (mkCoVar (mkSystemVarName minLocalUnique (mkFastString "False")) (LitTy (StrTyLit (mkFastString "Bool"))))

boolToExpression :: Bool -> Expr Var  --this is a wild hack, i just took the easiest constructors i found to create a "Var"-Instance without understanding what those constructors stand form 
boolToExpression True = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc ("True"))) (LitTy (StrTyLit (mkFastString "Bool"))) vanillaIdInfo)
boolToExpression False = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc ("False"))) (LitTy (StrTyLit (mkFastString "Bool"))) vanillaIdInfo)




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

showVarExpression :: Expr Var -> String
showVarExpression (Var var) = varToString var
showVarExpression _ = error "Expression is no var"

convertToMultiArgumentFunction :: Expr Var -> (Expr Var, [Expr Var])
convertToMultiArgumentFunction expr = collectArgs expr

varToString :: Var -> String
varToString var = nameToString (varName var)

nameToString :: Name -> String
nameToString name = showOutputable name

integerToCoreLiteral :: Integer -> Literal
integerToCoreLiteral value = mkLitInt64 value

integerToCoreExpression :: Integer -> Expr b
integerToCoreExpression value = Lit (mkLitInt64 value)

stringToCoreExpression :: String -> Expr Var
stringToCoreExpression value = Lit (mkLitString value)

coreLiteralToFractional :: Fractional a => Literal -> a
coreLiteralToFractional (LitFloat value) = fromRational value
coreLiteralToFractional (LitDouble value) = fromRational value

rationalToCoreExpression :: Rational -> Expr b
rationalToCoreExpression value = Lit (LitDouble value)

fractionalToCoreLiteral :: Real a => a -> Literal
fractionalToCoreLiteral value = (LitDouble (toRational value))

isInHeadNormalForm :: Expr Var -> Bool
isInHeadNormalForm exp = exprIsHNF exp

isTypeInformation :: Expr Var -> Bool
isTypeInformation (Type _) = True
isTypeInformation (Var name) = "$" `isPrefixOf` (varToString name)
isTypeInformation x = False

canBeReduced exp = if isTypeInformation exp
                    then False
                    else not (exprIsHNF exp)
