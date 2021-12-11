module OriginalCoreAST.CoreStepperHelpers.CoreLookup(tryFindBinding, findMatchingPattern, findBindingForString)
where

import OriginalCoreAST.CoreTypeClassInstances ()
import Data.Maybe (isNothing, fromJust, fromMaybe)
import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind, collectArgs)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString), mkLitInt64, mkLitString
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import OriginalCoreAST.CoreStepperHelpers.CoreTransformator(deepReplaceMultipleVarWithinExpression, convertToMultiArgumentFunction)
import OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced, isList, isListType)
import Utils (showOutputable)
import Debug.Trace(trace)
type Binding = (Var, Expr Var) --for example x = 2 (x is "var" and 2 is "expr")

tryFindBinding :: Var -> [Binding] -> Maybe (Expr Var)
tryFindBinding name bindings = tryFindBindingForStringIncludingOverrideFunctions (varToString name) bindings

tryFindBindingForStringIncludingOverrideFunctions :: String -> [Binding] -> Maybe (Expr Var)
tryFindBindingForStringIncludingOverrideFunctions name bindings = do
  let overrideBindings = tryFindBindingForString ("override'" ++ name) bindings
  if (isNothing overrideBindings)
    then (tryFindBindingForString name bindings)
    else overrideBindings



--should only be used for testing
findBindingForString :: String -> [Binding] -> Expr Var
findBindingForString name bindings = do
  let foundBinding = tryFindBindingForString name bindings
  fromMaybe (error ("binding not found : " ++ name)) foundBinding

tryFindBindingForString :: String -> [Binding] -> Maybe (Expr Var)
tryFindBindingForString key bindings = do
  let foundBindings = filter (\binding -> (==) (varToString (fst binding)) key) bindings
  if (length foundBindings) > 1 
    then error (("more than one binding with name '" ++ key) ++ "' was found. The stepper is not (yet) able to choose the right binding by checking the signature.")
    else if (length foundBindings) == 1
      then Just (snd (head foundBindings))
      else Nothing  

findMatchingPattern :: Expr Var -> [Alt Var] -> Maybe (Expr Var)
findMatchingPattern expression patterns = do
  let foundPattern = findMatchingPatternIgnoreDefault expression patterns
  case foundPattern of {
    (Just x) -> Just x;
    Nothing -> findMatchingDefaultPattern  expression patterns
  }

findMatchingPatternIgnoreDefault :: Expr Var -> [Alt Var] -> Maybe (Expr Var)
findMatchingPatternIgnoreDefault expression [] = {-trace "no matching pattern found" -}Nothing
findMatchingPatternIgnoreDefault (Var name) (((DataAlt dataCon), _, expression):xs) = if ((==) (varToString name) (showOutputable dataCon)) --check: is there a more elegant way than "show outputable"
                                                                                then Just expression
                                                                                else (findMatchingPatternIgnoreDefault (Var name) xs)
findMatchingPatternIgnoreDefault (Lit literal) (((LitAlt patternLiteral), _, expression):xs) = if ((==) literal patternLiteral) --can we compare two literals like this?
                                                                                             then Just expression
                                                                                             else (findMatchingPatternIgnoreDefault (Lit literal) xs)
findMatchingPatternIgnoreDefault (App expr argument) (((DataAlt patternConstructorName), boundNames, expression):xs) = do
    let (function, arguments) = convertToMultiArgumentFunction (App expr argument)
    if ((==) (varExpressionToString function) (showOutputable patternConstructorName)) --check: is there a more elegant way than "show outputable"
        then Just (deepReplaceMultipleVarWithinExpression boundNames (filter (not.isTypeInformation) arguments) expression)
        else (findMatchingPatternIgnoreDefault (App expr argument) xs)
findMatchingPatternIgnoreDefault expression (x:xs) = findMatchingPatternIgnoreDefault expression xs


findMatchingDefaultPattern :: Expr Var -> [Alt Var] -> Maybe (Expr Var)
findMatchingDefaultPattern expression [] = {-trace "no matching pattern found"-} Nothing
findMatchingDefaultPattern _ ((DEFAULT, _, expression):_) = Just expression
findMatchingDefaultPattern expression (x:xs) = findMatchingDefaultPattern expression xs
