module SimplifiedCoreAST.SimplifiedCoreASTReducer
  (printStepByStepReduction
  )
where

import SimplifiedCoreAST.SimplifiedCoreAST (ExpressionS(..), LiteralS(..), AltS(..), AltConS(..), BindS(..))
import Data.List(isPrefixOf, find)
import SimplifiedCoreAST.SimplifiedCoreASTPrinter (printSimplifiedCoreExpression)
import Data.Maybe

type ReductionStepDescription = String --for example: "replace x with definition"

printStepByStepReduction :: [BindS] -> BindS -> IO ()
printStepByStepReduction bindings (x, expression) = do
    putStr "\n**Reduction of "
    putStr x
    putStr "**"
    putStrLn ""
    printSimplifiedCoreExpression expression 0
    reduce bindings expression

{-Printing-}
reduce :: [BindS] -> ExpressionS -> IO ()
reduce bindings expression    | canBeReduced expression = do
                                let (reductionStepDescription, reducedExpression) = (applyStep bindings expression)
                                putStrLn ("\n{-" ++ reductionStepDescription ++ "-}")
                                printSimplifiedCoreExpression reducedExpression 0
                                reduce bindings reducedExpression
                              | otherwise = putStrLn "\n{-reduction complete-}"

canBeReduced :: ExpressionS -> Bool
canBeReduced (VarS _) = True
canBeReduced (AppS _ _) = True
canBeReduced _ = False

findBinding :: String -> [BindS] -> ExpressionS
findBinding key bindings = (tryFindBinding key bindings) ?? (InvalidExpression "Binding not Found")

tryFindBinding :: String -> [BindS] -> Maybe ExpressionS
tryFindBinding key bindings = do
    foundBinding <- find (\ (x, _) -> x == key) bindings
    return (snd foundBinding)

-- see https://stackoverflow.com/questions/47371950/maybe-coalescing-operator (copied code)
infixr 3 ??
(??) :: Maybe a -> a -> a
Just x ?? _ = x
Nothing ?? y = y



applyStep :: [BindS] -> ExpressionS -> (ReductionStepDescription, ExpressionS)
applyStep bindings (VarS name) = (("Replace '" ++ name ++ "' with definition"),(findBinding name bindings)) {-replace binding reference with actual expression (Delta Reduction)-}
applyStep bindings (AppS (LamS parameter expression) argument) = ("Application", deepReplaceVarWithinExpression parameter argument expression)
applyStep bindings (AppS (MultiArgumentAppS name arguments) argument) = ("Add argument to built-in multi-argument function", (MultiArgumentAppS name (arguments ++ [argument])))
applyStep bindings (AppS (AppS name firstArgument) secondArgument) = (description, (AppS simplifiedFirstApplication secondArgument))  --if the expression of the application is itself an application, the first application should be simplified
                                                                      where (description, simplifiedFirstApplication) = applyStep bindings (AppS name firstArgument)
applyStep bindings (AppS (VarS name) argument) = do
  let userDefinedExpression = tryFindBinding name bindings 
  if (isNothing (userDefinedExpression))
    then ("Convert to built-in multi-argument function. Please note that multi-argument-functions are not pure Haskell Core but used here to reduce functions that are not defined by the user itself.", (MultiArgumentAppS name [argument]))
    else ("Replace '" ++ name ++ "' with definition", (AppS (fromJust userDefinedExpression) argument))

applyStep bindings _  = ("ToDo: Implement Reduction", InvalidExpression "No reduction implemented for this type of expression")

deepReplaceVarWithinExpression :: String -> ExpressionS -> ExpressionS -> ExpressionS
deepReplaceVarWithinExpression name replaceExpression (VarS varName) = if ((==) varName name) then replaceExpression else (VarS varName)
deepReplaceVarWithinExpression name replaceExpression (AppS expression argument) = AppS (deepReplaceVarWithinExpression name replaceExpression expression) (deepReplaceVarWithinExpression name replaceExpression argument)
deepReplaceVarWithinExpression name replaceExpression (LamS parameter expression) = if (parameter == name) 
                                                                                      then (LamS parameter expression) --do nothing, use local lamda parameter with the same name
                                                                                      else (LamS parameter (deepReplaceVarWithinExpression name replaceExpression expression))
deepReplaceVarWithinExpression name replaceExpression (CaseS expression alternatives) = CaseS (deepReplaceVarWithinExpression name replaceExpression expression) (map (deepReplaceVarWithinAlternative name replaceExpression) alternatives)
deepReplaceVarWithinExpression name replaceExpression (MultiArgumentAppS appName arguments) = (MultiArgumentAppS appName (map (deepReplaceVarWithinExpression name replaceExpression) arguments))
deepReplaceVarWithinExpression _ _ expression = expression --nothing to replace


deepReplaceVarWithinAlternative :: String -> ExpressionS -> AltS-> AltS 
deepReplaceVarWithinAlternative name replaceExpression (altCon, localBoundStrings, expression) = if (elem name localBoundStrings)
                                                                                                  then (altCon, localBoundStrings, expression) --do nothing, use local lamda parameter with the same name
                                                                                                  else (altCon, localBoundStrings, (deepReplaceVarWithinExpression name replaceExpression expression))

sum :: ExpressionS -> ExpressionS -> ExpressionS
sum _ _ = LitS (LitNumberS 4)

--verschiedene Arten  Function Application

--Expression ist ein Lamda => Wert einsetzen
--Expression ist eine MultiFunctionApplication => Das Argument zur Liste der Parameter hinzufügen
-- Expression ist eine Application => Expression vereinfachen

--Expression ist ein Var
    --Hinter dem Var ist ein Lamda => Var durch Lamda ersetzen
    --Hinter dem Var ist ein Operator oder eine Funktion aus der Prelude
        --App in ein MultiFunctionApplication umwandeln 

-- data ExpressionS
--     = VarS String --for example "x" or "+"
--     | LitS LiteralS --for example "4"
--     | AppS {expressionS:: ExpressionS, argumentS :: ExpressionS} --for example "+ 1"
--     | LamS {parameterS:: String, expressionS:: ExpressionS} --for example "\x -> ...""
--     | CaseS {expressionS:: ExpressionS, alternativesS:: [AltS]} --for example "\a -> case (== a 1) of {true -> "One" false -> "not one"};"
--     | TypeS --not implemented
--     | MultiArgumentAppS {name:: String, argumentsS :: [ExpressionS]} --not in original core but used so we can make a reduction with build-in functions/operators from the prelude like (+ 1) 2 -> (+ 1 2) -> 3 
--     | InvalidExpression String --for example "unsupported expression"


-- vier Arten von Funktionen: integrierte direkt evaluierte, über code integrierte, vom user bereitgestellte, unbekannte 


-- wie kann ich Dinge wie (+1) 2 vereinfachen wenn jede Funktion immer nur ein Parameter haben kann?
-- kann ich zum Beispiel schreiben (+ 1 2)?
-- das ist ein Argument für den simplified AST