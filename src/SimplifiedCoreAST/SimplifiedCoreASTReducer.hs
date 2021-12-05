module SimplifiedCoreAST.SimplifiedCoreASTReducer
  ( printStepByStepReduction,
  )
where

import Data.List (find, isPrefixOf)
import Data.Maybe
import SimplifiedCoreAST.SimplifiedCoreAST (AltConS (..), AltS (..), BindS (..), ExpressionS (..), LiteralS (..))
import SimplifiedCoreAST.SimplifiedCoreASTPrinter (printSimplifiedCoreExpression)

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
reduce bindings expression
  | canBeReduced bindings expression = do
    let (reductionStepDescription, reducedExpression) = applyStep bindings expression
    putStrLn ("\n{-" ++ reductionStepDescription ++ "-}")
    printSimplifiedCoreExpression reducedExpression 0
    reduce bindings reducedExpression
  | otherwise = putStrLn "\n{-reduction complete-}"

canBeReduced :: [BindS] -> ExpressionS -> Bool
canBeReduced bindings (VarS name) =
  if isNothing (tryFindBinding name bindings)
    then False
    else True
canBeReduced _ (AppS _ _) = True
canBeReduced _ (MultiArgumentAppS name arguments) =
  if isConstructor name
    then False
    else True
canBeReduced _ (CaseS _ _) = True
canBeReduced _ _ = False

isConstructor :: String -> Bool
isConstructor ":" = True
isConstructor _ = False

--ToDo: add more well known constructors

findBinding :: String -> [BindS] -> ExpressionS
findBinding key bindings = tryFindBinding key bindings ?? InvalidExpression "Binding not Found"

tryFindBinding :: String -> [BindS] -> Maybe ExpressionS
tryFindBinding key bindings = do
  foundBinding <- find (\(x, _) -> x == key) bindings
  return (snd foundBinding)

-- see https://stackoverflow.com/questions/47371950/maybe-coalescing-operator (copied code)
infixr 3 ??

(??) :: Maybe a -> a -> a
Just x ?? _ = x
Nothing ?? y = y

applyStep :: [BindS] -> ExpressionS -> (ReductionStepDescription, ExpressionS)
applyStep bindings (VarS name) = ("Replace '" ++ name ++ "' with definition", (findBinding name bindings {-replace binding reference with actual expression (Delta Reduction)-}))
applyStep bindings (AppS (LamS parameter expression) argument) = ("Lamda Application", deepReplaceVarWithinExpression parameter argument expression)
applyStep bindings (AppS (MultiArgumentAppS name arguments) argument) =
  if canBeReduced bindings argument
    then (description {-++ " (please note that this is an eager reduction and not default GHC behaviour. This approach is chosen because this expression will be used as a parameter for an application that can not be further stepped or where further stepping ist not yet supported)"-}, (AppS (MultiArgumentAppS name arguments) reducedArgument))
    else ("Add argument to built-in multi-argument function", MultiArgumentAppS name (arguments ++ [argument]))
  where
    (description, reducedArgument) = applyStep bindings argument
applyStep bindings (AppS (AppS name firstArgument) secondArgument) = (description, AppS simplifiedFirstApplication secondArgument) --if the expression of the application is itself an application, the first application should be simplified
  where
    (description, simplifiedFirstApplication) = applyStep bindings (AppS name firstArgument)
applyStep bindings (AppS (VarS name) argument) = do
  let userDefinedExpression = tryFindBinding name bindings
  if (isNothing userDefinedExpression)
    then
      if canBeReduced bindings argument
        then (description, AppS (VarS name) reducedArgument)
        else ("Convert to built-in multi-argument function. Please note that multi-argument-functions are not pure Haskell Core but used here to reduce functions that are not defined by the user itself.", (MultiArgumentAppS name [argument]))
    else ("Replace '" ++ name ++ "' with definition", AppS (fromJust userDefinedExpression) argument)
  where
    (description, reducedArgument) = applyStep bindings argument
applyStep bindings (MultiArgumentAppS name arguments) = ("apply " ++ name ++ " (note: showing substeps is not possible or implemented for this function)", applyFunction name arguments)
applyStep bindings (CaseS expression alternatives) =
  if canBeReduced bindings expression
    then (description, CaseS reducedExpression alternatives)
    else ("Replace with matching pattern", findMatchingPattern expression alternatives)
  where
    (description, reducedExpression) = applyStep bindings expression
applyStep bindings _ = ("ToDo: Implement Reduction", InvalidExpression "No reduction implemented for this type of expression")

findMatchingPattern :: ExpressionS -> [AltS] -> ExpressionS
findMatchingPattern expression [] = InvalidExpression "No matching pattern was found or this type of pattern is not yet supported"
findMatchingPattern _ ((DefaultS, _, expression) : xs) = expression --default match
findMatchingPattern (VarS name) ((DataAltS patternConstructorName, _, expression) : xs) =
  if (==) name patternConstructorName
    then expression
    else findMatchingPattern (VarS name) xs
findMatchingPattern (LitS literal) ((LitAltS patternLiteral, _, expression) : xs) =
  if (==) literal patternLiteral
    then expression
    else findMatchingPattern (LitS literal) xs
findMatchingPattern (MultiArgumentAppS name arguments) ((DataAltS patternConstructorName, boundNames, expression) : xs) =
  if (==) name patternConstructorName
    then deepReplaceMultipleVarWithinExpression boundNames arguments expression
    else findMatchingPattern (MultiArgumentAppS name arguments) xs
findMatchingPattern expression (x : xs) = findMatchingPattern expression xs

deepReplaceMultipleVarWithinExpression :: [String] -> [ExpressionS] -> ExpressionS -> ExpressionS
deepReplaceMultipleVarWithinExpression [] _ expression = expression
deepReplaceMultipleVarWithinExpression _ [] expression = expression
deepReplaceMultipleVarWithinExpression (x : xs) (y : ys) expression = deepReplaceMultipleVarWithinExpression xs ys (deepReplaceVarWithinExpression x y expression)

deepReplaceVarWithinExpression :: String -> ExpressionS -> ExpressionS -> ExpressionS
deepReplaceVarWithinExpression name replaceExpression (VarS varName) = if (==) varName name then replaceExpression else VarS varName
deepReplaceVarWithinExpression name replaceExpression (AppS expression argument) = AppS (deepReplaceVarWithinExpression name replaceExpression expression) (deepReplaceVarWithinExpression name replaceExpression argument)
deepReplaceVarWithinExpression name replaceExpression (LamS parameter expression) =
  if parameter == name
    then LamS parameter expression --do nothing, use local lamda parameter with the same name
    else LamS parameter (deepReplaceVarWithinExpression name replaceExpression expression)
deepReplaceVarWithinExpression name replaceExpression (CaseS expression alternatives) = CaseS (deepReplaceVarWithinExpression name replaceExpression expression) (map (deepReplaceVarWithinAlternative name replaceExpression) alternatives)
deepReplaceVarWithinExpression name replaceExpression (MultiArgumentAppS appName arguments) = MultiArgumentAppS appName (map (deepReplaceVarWithinExpression name replaceExpression) arguments)
deepReplaceVarWithinExpression _ _ expression = expression --nothing to replace

deepReplaceVarWithinAlternative :: String -> ExpressionS -> AltS -> AltS
deepReplaceVarWithinAlternative name replaceExpression (altCon, localBoundStrings, expression) =
  if name `elem` localBoundStrings
    then (altCon, localBoundStrings, expression) --do nothing, use local lamda parameter with the same name
    else (altCon, localBoundStrings, deepReplaceVarWithinExpression name replaceExpression expression)

applyFunction :: String -> [ExpressionS] -> ExpressionS
applyFunction "+" [x, y] = (+) x y
applyFunction "-" [x, y] = (-) x y
applyFunction "*" [x, y] = (*) x y
applyFunction "/" [x, y] = (/) x y
applyFunction "recip" [x] = recip x
applyFunction "signum" [x] = signum x
applyFunction "abs" [x] = abs x
applyFunction "/=" [x, y] = boolToExpression ((/=) x y)
applyFunction "==" [x, y] = boolToExpression ((==) x y)
applyFunction "<" [x, y] = boolToExpression ((<) x y)
applyFunction ">" [x, y] = boolToExpression ((>) x y)
applyFunction ">=" [x, y] = boolToExpression ((>=) x y)
applyFunction "<=" [x, y] = boolToExpression ((<=) x y)
applyFunction "negate" [LitS (LitNumberS x)] = integerToExpression (negate x) --example of an arbitrary function from the prelude. note how the arguments must have the right type and the result is converted back into an expressino
applyFunction name _ = InvalidExpression (name ++ " not supported (yet)")

-- toDo: Add more functions from the prelude

boolToExpression :: Bool -> ExpressionS
boolToExpression True = VarS "True"
boolToExpression False = VarS "False"

integerToExpression :: Integer -> ExpressionS
integerToExpression x = LitS (LitNumberS x)
