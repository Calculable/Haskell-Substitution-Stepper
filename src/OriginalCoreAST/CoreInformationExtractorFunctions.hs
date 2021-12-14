module OriginalCoreAST.CoreInformationExtractorFunctions (varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced, isList, isMaybe, isNothingMaybe, isJustMaybe, isListType, isEmptyList, isVarExpression, isClassDictionary, getFunctionOfNestedApplication, typeOfExpression, isIntType, isBoolType, isCharType, boolValueFromVar, isBoolVar) where

import Data.List (isPrefixOf)
import GHC.Plugins
  ( Expr (..),
    Literal (LitDouble, LitFloat),
    Name,
    Type,
    Var (varName),
    collectArgs,
    exprIsHNF,
    getOccString,
  )
import Utils (showOutputable)

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

isTypeInformation :: Expr Var -> Bool
isTypeInformation (Type _) = True
isTypeInformation (App expr arg) = isTypeInformation expr
isTypeInformation x = isClassDictionary x

isClassDictionary :: Expr Var -> Bool
isClassDictionary (Var name) = "$" `isPrefixOf` varToString name
isClassDictionary (App expr args) = isClassDictionary (getFunctionOfNestedApplication (App expr args))
isClassDictionary x = False

isVarExpression :: Expr Var -> Bool
isVarExpression (Var name) = True
isVarExpression _ = False

-- | The "canBeReducedFunction" checks if a Core expression is not yet in head normal form and can further be reduced
canBeReduced :: Expr Var -> Bool
canBeReduced exp
  | isTypeInformation exp = False
  | isBoolVar exp = False
  | otherwise = case exp of --check nested application
    (App (Lam _ _) x) -> True
    (App (Let _ _) x) -> True
    (Case {}) -> True
    (Cast _ _) -> True
    (Let _ _) -> True
    (App x y) -> canBeReduced (getFunctionOfNestedApplication (App x y)) || not (exprIsHNF exp)
    _ -> not (exprIsHNF exp)

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

isNonEmptyList :: Expr a -> Bool --can this be checked more elegant?
isNonEmptyList expr = isConstructorApplicationOfType expr ":"

isEmptyList :: Expr a -> Bool
isEmptyList expr = isConstructorApplicationOfType expr "[]"

isList :: Expr a -> Bool
isList expr = (||) (isNonEmptyList expr) (isEmptyList expr)

isNothingMaybe :: Expr a -> Bool --can this be checked more elegant?
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

isListType :: Expr a -> Bool --is there a more elegant solution?
isListType (Type ty) = showOutputable ty == "[]"
isListType _ = False

isIntType :: Type -> Bool --is there a more elegant solution?
isIntType ty = showOutputable ty == "Int"

isBoolType :: Type -> Bool --is there a more elegant solution?
isBoolType ty = showOutputable ty == "Bool"

isCharType :: Type -> Bool --is there a more elegant solution?
isCharType ty = showOutputable ty == "Char"

getFunctionOfNestedApplication :: Expr Var -> Expr Var
getFunctionOfNestedApplication expr = fst (collectArgs expr)

typeOfExpression :: Expr Var -> String --used for tracing / debugging
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