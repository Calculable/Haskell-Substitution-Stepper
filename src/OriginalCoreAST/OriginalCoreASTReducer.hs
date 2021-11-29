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
import GHC.Types.Var (Var (varName, varType), TyVar, Id)
import GHC.Utils.Outputable (Outputable (ppr), OutputableBndr)
import GHC.Core.Utils (exprIsHNF)
import Utils (showOutputable)
import GHC.Core.Ppr
  ( pprCoreAlt,
  )
import Data.List(isPrefixOf)
import SimplifiedCoreAST.SimplifiedCoreAST (ExpressionS(..), LiteralS(..), AltS(..), AltConS(..), BindS(..))
import GHC.Types.Name(nameUnique, Name)


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
                            else Just ("Apply " ++ (showOutputable function), applyFunctionToArguments function arguments) --all arguments are reduced, eval function. This is stric behaviour! We have to use strict behaviour here because we are trying to evaluate a function whose definition we do not know. therefor we cannot apply the arguments one after another but have to simplify all arguments before calling the function 
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

applyFunctionToArguments :: Expr Var -> [Expr Var] -> Expr Var 
applyFunctionToArguments expr arguments = stringToCoreExpression ("ToDo: Implement " ++ showOutputable expr ) --toDo: Implement


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

integerToCoreExpression :: Integer -> Expr Var
integerToCoreExpression value = Lit (mkLitInt64 value)

stringToCoreExpression :: String -> Expr Var
stringToCoreExpression value = Lit (mkLitString value)

coreLiteralToFractional :: Fractional a => Literal -> a
coreLiteralToFractional (LitFloat value) = fromRational value
coreLiteralToFractional (LitDouble value) = fromRational value

fractionalToCoreLiteral :: Real a => a -> Literal
fractionalToCoreLiteral value =  (LitDouble (toRational value))

isInHeadNormalForm :: Expr Var -> Bool
isInHeadNormalForm exp = exprIsHNF exp

isTypeInformation :: Expr Var -> Bool
isTypeInformation (Type _) = True
isTypeInformation (Var name) = "$" `isPrefixOf` (varToString name)
isTypeInformation x = False

canBeReduced exp = if isTypeInformation exp
                    then False
                    else not (exprIsHNF exp)

-- reduce :: [BindS] -> ExpressionS -> IO ()
-- reduce bindings expression    | canBeReduced bindings expression = do
--                                 let (reductionStepDescription, reducedExpression) = (applyStep bindings expression)
--                                 putStrLn ("\n{-" ++ reductionStepDescription ++ "-}")
--                                 printSimplifiedCoreExpression reducedExpression 0
--                                 reduce bindings reducedExpression
--                               | otherwise = putStrLn "\n{-reduction complete-}"

-- canBeReduced :: [BindS] -> ExpressionS -> Bool
-- canBeReduced bindings (VarS name) = if (isNothing (tryFindBinding name bindings))
--                               then False
--                               else True
-- canBeReduced _ (AppS _ _) = True
-- canBeReduced _ (MultiArgumentAppS name arguments) = if (isConstructor name) 
--                                                       then False
--                                                       else True
-- canBeReduced _ (CaseS _ _) = True
-- canBeReduced _ _ = False

-- isConstructor :: String -> Bool
-- isConstructor ":" = True
-- isConstructor _ = False
-- --ToDo: add more well known constructors

-- findBinding :: String -> [BindS] -> ExpressionS
-- findBinding key bindings = (tryFindBinding key bindings) ?? (InvalidExpression "Binding not Found")

-- tryFindBinding :: String -> [BindS] -> Maybe ExpressionS
-- tryFindBinding key bindings = do
--     foundBinding <- find (\ (x, _) -> x == key) bindings
--     return (snd foundBinding)

-- -- see https://stackoverflow.com/questions/47371950/maybe-coalescing-operator (copied code)
-- infixr 3 ??
-- (??) :: Maybe a -> a -> a
-- Just x ?? _ = x
-- Nothing ?? y = y



-- applyStep :: [BindS] -> ExpressionS -> (ReductionStepDescription, ExpressionS)
-- applyStep bindings (VarS name) = (("Replace '" ++ name ++ "' with definition"),(findBinding name bindings)) {-replace binding reference with actual expression (Delta Reduction)-}
-- applyStep bindings (AppS (LamS parameter expression) argument) = ("Lamda Application", deepReplaceVarWithinExpression parameter argument expression)
-- applyStep bindings (AppS (MultiArgumentAppS name arguments) argument) = if (canBeReduced bindings argument)
--                                                                           then (description {-++ " (please note that this is an eager reduction and not default GHC behaviour. This approach is chosen because this expression will be used as a parameter for an application that can not be further stepped or where further stepping ist not yet supported)"-}, (AppS (MultiArgumentAppS name arguments) reducedArgument))
--                                                                           else ("Add argument to built-in multi-argument function", (MultiArgumentAppS name (arguments ++ [argument])))
--                                                                           where (description, reducedArgument) = applyStep bindings argument
-- applyStep bindings (AppS (AppS name firstArgument) secondArgument) = (description, (AppS simplifiedFirstApplication secondArgument))  --if the expression of the application is itself an application, the first application should be simplified
--                                                                       where (description, simplifiedFirstApplication) = applyStep bindings (AppS name firstArgument)
-- applyStep bindings (AppS (VarS name) argument) = do
--   let userDefinedExpression = tryFindBinding name bindings 
--   if (isNothing (userDefinedExpression))
--     then
--       if (canBeReduced bindings argument)
--         then (description, (AppS (VarS name) (reducedArgument))) 
--         else ("Convert to built-in multi-argument function. Please note that multi-argument-functions are not pure Haskell Core but used here to reduce functions that are not defined by the user itself.", (MultiArgumentAppS name [argument]))
--     else ("Replace '" ++ name ++ "' with definition", (AppS (fromJust userDefinedExpression) argument))
--   where (description, reducedArgument) = applyStep bindings argument 
-- applyStep bindings (MultiArgumentAppS name arguments) = ("apply " ++ name ++ " (note: showing substeps is not possible or implemented for this function)", applyFunction name arguments)
-- applyStep bindings (CaseS expression alternatives) = if (canBeReduced bindings expression)
--                                                       then (description, (CaseS reducedExpression alternatives))
--                                                       else ("Replace with matching pattern", findMatchingPattern expression alternatives)
--                                                       where (description, reducedExpression) = applyStep bindings expression

-- applyStep bindings _  = ("ToDo: Implement Reduction", InvalidExpression "No reduction implemented for this type of expression")

-- findMatchingPattern :: ExpressionS -> [AltS] -> ExpressionS
-- findMatchingPattern expression [] = InvalidExpression "No matching pattern was found or this type of pattern is not yet supported"
-- findMatchingPattern _ ((DefaultS, _, expression):xs) = expression --default match
-- findMatchingPattern (VarS name) (((DataAltS patternConstructorName), _, expression):xs) = if ((==) name patternConstructorName)
--                                                                                             then expression
--                                                                                             else (findMatchingPattern (VarS name) xs)
-- findMatchingPattern (LitS literal) (((LitAltS patternLiteral), _, expression):xs) = if ((==) literal patternLiteral)
--                                                                                             then expression
--                                                                                             else (findMatchingPattern (LitS literal) xs)
-- findMatchingPattern (MultiArgumentAppS name arguments) (((DataAltS patternConstructorName), boundNames, expression):xs) = if ((==) name patternConstructorName)
--                                                                                                                             then deepReplaceMultipleVarWithinExpression boundNames arguments expression
--                                                                                                                             else (findMatchingPattern (MultiArgumentAppS name arguments) xs)
-- findMatchingPattern expression (x:xs) = findMatchingPattern expression xs


-- deepReplaceMultipleVarWithinExpression :: [String] -> [ExpressionS] -> ExpressionS -> ExpressionS
-- deepReplaceMultipleVarWithinExpression [] _ expression = expression
-- deepReplaceMultipleVarWithinExpression _ [] expression = expression
-- deepReplaceMultipleVarWithinExpression (x:xs) (y:ys) expression = deepReplaceMultipleVarWithinExpression xs ys (deepReplaceVarWithinExpression x y expression)

-- deepReplaceVarWithinExpression :: String -> ExpressionS -> ExpressionS -> ExpressionS
-- deepReplaceVarWithinExpression name replaceExpression (VarS varName) = if ((==) varName name) then replaceExpression else (VarS varName)
-- deepReplaceVarWithinExpression name replaceExpression (AppS expression argument) = AppS (deepReplaceVarWithinExpression name replaceExpression expression) (deepReplaceVarWithinExpression name replaceExpression argument)
-- deepReplaceVarWithinExpression name replaceExpression (LamS parameter expression) = if (parameter == name) 
--                                                                                       then (LamS parameter expression) --do nothing, use local lamda parameter with the same name
--                                                                                       else (LamS parameter (deepReplaceVarWithinExpression name replaceExpression expression))
-- deepReplaceVarWithinExpression name replaceExpression (CaseS expression alternatives) = CaseS (deepReplaceVarWithinExpression name replaceExpression expression) (map (deepReplaceVarWithinAlternative name replaceExpression) alternatives)
-- deepReplaceVarWithinExpression name replaceExpression (MultiArgumentAppS appName arguments) = (MultiArgumentAppS appName (map (deepReplaceVarWithinExpression name replaceExpression) arguments))
-- deepReplaceVarWithinExpression _ _ expression = expression --nothing to replace


-- deepReplaceVarWithinAlternative :: String -> ExpressionS -> AltS-> AltS 
-- deepReplaceVarWithinAlternative name replaceExpression (altCon, localBoundStrings, expression) = if (elem name localBoundStrings)
--                                                                                                   then (altCon, localBoundStrings, expression) --do nothing, use local lamda parameter with the same name
--                                                                                                   else (altCon, localBoundStrings, (deepReplaceVarWithinExpression name replaceExpression expression))
-- applyFunction :: String -> [ExpressionS] -> ExpressionS
-- applyFunction "+" [x, y] = (+) x y
-- applyFunction "-" [x, y] = (-) x y
-- applyFunction "*" [x, y] = (*) x y
-- applyFunction "/" [x, y] = (/) x y
-- applyFunction "recip" [x] = signum x
-- applyFunction "signum" [x] = signum x
-- applyFunction "abs" [x] = abs x
-- applyFunction "/=" [x, y] = boolToExpression ((/=) x y)
-- applyFunction "==" [x, y] = boolToExpression ((==) x y)
-- applyFunction "<" [x, y] = boolToExpression ((<) x y)
-- applyFunction ">" [x, y] = boolToExpression ((>) x y)
-- applyFunction ">=" [x, y] = boolToExpression ((>=) x y)
-- applyFunction "<=" [x, y] = boolToExpression ((<=) x y)
-- applyFunction "negate" [(LitS (LitNumberS x))] = integerToExpression (negate x) --example of an arbitrary function from the prelude. note how the arguments must have the right type and the result is converted back into an expressino
-- applyFunction name _ = InvalidExpression (name ++ " not supported (yet)") 
-- -- toDo: Add more functions from the prelude

-- boolToExpression :: Bool -> ExpressionS
-- boolToExpression True = VarS "True"
-- boolToExpression False = VarS "False"

-- integerToExpression :: Integer -> ExpressionS
-- integerToExpression x = LitS (LitNumberS x)
