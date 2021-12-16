module OriginalCoreAST.CoreInformationExtractorFunctions (varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced, isList, isMaybe, isNothingMaybe, isJustMaybe, isListType, isTupleType, isEmptyList, isNonEmptyTuple, isEmptyTuple, isTuple, isVarExpression, isClassDictionary, getFunctionOfNestedApplication, typeOfExpression, isIntType, isBoolType, isCharType, boolValueFromVar, isBoolVar, removeTypeInformation, getIndividualElementsOfList, getIndividualElementsOfTuple, isPrimitiveTypeConstructorApp, getLiteralArgument, isPrimitiveTypeConstructorName, isTypeWrapperFunctionName) where

import Data.List (isPrefixOf, isSuffixOf)
import GHC.Plugins
  ( Expr (..),
    Literal (LitDouble, LitFloat),
    Name,
    Type,
    Var (varName),
    collectArgs,
    exprIsHNF,
    getOccString,
    varType
  )
import Utils (showOutputable)
import Debug.Trace(trace)
import OriginalCoreAST.CoreStepperHelpers.TracerHelper
import GHC.Core.TyCo.Rep (Type(..))

varExpressionToString :: Expr Var -> String
varExpressionToString (Var var) = varToString var
varExpressionToString _ = error "Expression is no var"

varToString :: Var -> String
varToString var =
  if isBoolVar (Var var) --find better solution for boolean workaround
    then varToSimpleString var
    else showOutputable var

coreLiteralToFractional :: Fractional a => Literal -> a
coreLiteralToFractional (LitFloat value) = fromRational value
coreLiteralToFractional (LitDouble value) = fromRational value

isInHeadNormalForm :: Expr Var -> Bool
isInHeadNormalForm = exprIsHNF

isTypeInformation :: Expr b -> Bool
isTypeInformation (Type _) = True
--isTypeInformation (App expr arg) = isTypeInformation expr
isTypeInformation x = isClassDictionary x

isClassDictionary :: Expr b -> Bool
isClassDictionary (Var name) = "$" `isPrefixOf` varName && not (varName == "$" || varName == "$!") --todo: ugly, find better solution
  where varName = varToString name
isClassDictionary (App expr args) = isClassDictionary (getFunctionOfNestedApplication (App expr args))
isClassDictionary x = False

isTyConAppType :: Type -> Bool
isTyConAppType (TyConApp _ _) = True
isTyConAppType _ = False

isTypeWrapperFunctionName :: String -> Bool
isTypeWrapperFunctionName name = "#" `isSuffixOf` name


isVarExpression :: Expr Var -> Bool
isVarExpression (Var name) = True
isVarExpression _ = False

-- | The "canBeReducedFunction" checks if a Core expression is not yet in head normal form and can further be reduced
canBeReduced :: Expr Var -> Bool
canBeReduced exp
  | isBoolVar exp = False
  | isSupportedVar exp = True
  | isTypeInformation exp = False --toDo: ignore when it is inside app...
  | otherwise = case exp of --check nested application
    (App (Lam _ _) x) -> True
    (App (Let _ _) x) -> True
    (Case {}) -> True
    (Cast _ _) -> True
    (Let _ _) -> True
    (App x y) -> canBeReduced (getFunctionOfNestedApplication (App x y)) || not (exprIsHNF exp)
    _ -> not (exprIsHNF exp)

isSupportedVar :: Expr Var -> Bool
isSupportedVar (Var var) = isTypeWrapperFunctionName (varToString var)
isSupportedVar _ = False

isBoolVar :: Expr Var -> Bool
isBoolVar (Var x) = isBoolVarTrue x || isBoolVarFalse x
isBoolVar _ = False

boolValueFromVar :: Var -> Bool
boolValueFromVar x | isBoolVarTrue x = True
                   | isBoolVarFalse x = False
                   | otherwise = error "the provided variable does not represent a bool" 


isBoolVarTrue :: Var -> Bool
isBoolVarTrue x = (==) (varToSimpleString x) "True"

isBoolVarFalse :: Var -> Bool
isBoolVarFalse x = (==) (varToSimpleString x) "False"

varToSimpleString :: Var -> String
varToSimpleString var = nameToString (varName var)

nameToString :: Name -> String
nameToString = getOccString

isNonEmptyList :: Expr a -> Bool --can this be checked more elegantly?
isNonEmptyList expr = isConstructorApplicationOfType expr ":"

isEmptyList :: Expr a -> Bool
isEmptyList expr = isConstructorApplicationOfType expr "[]"

isList :: Expr a -> Bool
isList expr = (||) (isNonEmptyList expr) (isEmptyList expr)

isNonEmptyTuple :: Expr a -> Bool --can this be checked more elegantly?
isNonEmptyTuple (App expr arg) = do
  let (function, arguments) = collectArgs (App expr arg)
  case function of
    (Var var) -> (("(" `isPrefixOf` constructorName) && (")" `isSuffixOf` constructorName)) && (',' `elem` constructorName)
      where constructorName = (varToString var)
isNonEmptyTuple _ = False

isEmptyTuple :: Expr a -> Bool  --can this be checked more elegantly?
isEmptyTuple (Var var) = (varToString var) == "()"
isEmptyTuple _ = False

isTuple :: Expr a -> Bool
isTuple expr = (||) (isNonEmptyTuple expr) (isEmptyTuple expr)

isNothingMaybe :: Expr a -> Bool --can this be checked more elegantly?
isNothingMaybe expr = isConstructorApplicationOfType expr "Nothing"

isJustMaybe :: Expr a -> Bool
isJustMaybe expr = isConstructorApplicationOfType expr "Just"

isMaybe :: Expr a -> Bool
isMaybe expr = (||) (isNothingMaybe expr) (isJustMaybe expr)

isConstructorApplicationOfType :: Expr a -> String -> Bool
isConstructorApplicationOfType (App expr arg) name = do
  let (function, arguments) = collectArgs (App expr arg)
  case function of
    (Var var) -> (==) (varToString var) name
isConstructorApplicationOfType _ _ = False

isListType :: Type -> Bool --is there a more elegant solution?
isListType ty = "[" `isPrefixOf` typeRepresentation && "]" `isSuffixOf` typeRepresentation
  where typeRepresentation = showOutputable ty

isTupleType :: Type -> Bool --is there a more elegant solution?
isTupleType ty = "(" `isPrefixOf` typeRepresentation && ")" `isSuffixOf` typeRepresentation
  where typeRepresentation = showOutputable ty

isIntType :: Type -> Bool --is there a more elegant solution?
isIntType ty = showOutputable ty == "Int"

isBoolType :: Type -> Bool --is there a more elegant solution?
isBoolType ty = showOutputable ty == "Bool"

isCharType :: Type -> Bool --is there a more elegant solution?
isCharType ty = showOutputable ty == "Char"

getFunctionOfNestedApplication :: Expr b -> Expr b
getFunctionOfNestedApplication expr = fst (collectArgs expr)

typeOfExpression :: Expr a -> String --used for tracing / debugging
typeOfExpression (Var _) = "Var"
typeOfExpression (Lit _) = "Lit"
typeOfExpression (App _ _) = "App"
typeOfExpression (Lam _ _) = "Lam"
typeOfExpression (Let _ _) = "Let"
typeOfExpression (Case {}) = "Case"
typeOfExpression (Cast _ _) = "Cast"
typeOfExpression (Tick _ _) = "Tick"
typeOfExpression (Type _) = "Type"
typeOfExpression (Coercion _) = "Coercion"

isPrimitiveTypeConstructorApp :: Expr a -> Bool
isPrimitiveTypeConstructorApp (App (Var var) (Lit literal)) = isPrimitiveTypeConstructorVar var
isPrimitiveTypeConstructorApp _ = False

isPrimitiveTypeConstructorVar :: Var -> Bool
isPrimitiveTypeConstructorVar var = "#" `isSuffixOf` (varToString var)

isPrimitiveTypeConstructorName :: Name -> Bool
isPrimitiveTypeConstructorName name = "#" `isSuffixOf` (showOutputable name)

getLiteralArgument  :: Expr a -> Expr a
getLiteralArgument (App (Var var) (Lit literal)) = (Lit literal)


removeTypeInformation :: [Expr b] -> [Expr b]
removeTypeInformation list = filter (not . isTypeInformation) list

getIndividualElementsOfList :: Expr b -> [Expr b]
getIndividualElementsOfList expr
  | isEmptyList expr = []
  | isList expr = do
    let (constructor, elements) = collectArgs expr
    if length elements /= 3
      then error ("unexpected number of arguments to cons operator: " ++ show (length elements))
      else do
        let [ty, first, nestedList] = take 3 elements
        [ty, first] ++ getIndividualElementsOfList nestedList
  | otherwise = error "expression is not a list"

getIndividualElementsOfTuple :: Expr b -> [Expr b]
getIndividualElementsOfTuple expr
  | isEmptyTuple expr = []
  | isTuple expr = do
    let (constructor, elements) = collectArgs expr
    let values = snd (split elements)
    values
  | otherwise = error "expression is not a tuple"

{-this function is taken from: https://stackoverflow.com/questions/19074520/how-to-split-a-list-into-two-in-haskell-}
split :: [a] -> ([a], [a])
split myList = splitAt (((length myList) + 1) `div` 2) myList