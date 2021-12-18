module OriginalCoreAST.CoreInformationExtractorFunctions (varToString, nameToString, isTypeInformation, canBeReduced, isList, isListType, isEmptyList, isNonEmptyTuple, isEmptyTuple, isTuple, isVarExpression, isClassDictionary, getFunctionOfNestedApplication, isIntType, isBoolType, isCharType, boolValueFromVar, isBoolVar, removeTypeInformation, getIndividualElementsOfList, getIndividualElementsOfTuple, isPrimitiveTypeConstructorApp, isPrimitiveTypeConstructorName, getLiteralArgument, isTypeWrapperFunctionName, canBeReducedToNormalForm, varRefersToUnsteppableFunction, varsHaveTheSameName, varNameEqualsString, varsHaveTheSameType, isApplicationWithClassDictionary) where

import Data.List
import GHC.Plugins
import Utils
import GHC.Core.TyCo.Rep

unsteppableFunctionPrefix = "unsteppableFunction'"

{-Var Functions-}

varToString :: Var -> String
varToString var =
  if isBoolVar (Var var) --find better solution for boolean workaround
    then varToSimpleString var
    else showOutputable var
    
isVarExpression :: Expr b -> Bool
isVarExpression (Var name) = True
isVarExpression _ = False

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

varToSimpleString :: Var -> String
varToSimpleString var = nameToString (varName var)

nameToString :: Name -> String
nameToString = getOccString

varRefersToUnsteppableFunction :: Var -> Bool
varRefersToUnsteppableFunction name = unsteppableFunctionPrefix `isPrefixOf` varToString name

varsHaveTheSameName :: Var -> Var -> Bool 
varsHaveTheSameName x y = (==) (varToString x) (varToString y)

varsHaveTheSameType :: Var -> Var -> Bool 
varsHaveTheSameType x y = (==) (showOutputable (varType x)) (showOutputable (varType y))

varNameEqualsString :: Var -> String -> Bool
varNameEqualsString var name = (==) (varToString var) name

{-Reduction functions-}

-- | The "canBeReducedFunction" checks if a Core expression is not yet in normal form and can further be reduced
canBeReducedToNormalForm :: Expr Var -> Bool
canBeReducedToNormalForm (App expr argument) = do
  let (function, arguments) = collectArgs (App expr argument)
  (any canBeReduced arguments || any canBeReducedToNormalForm arguments) || canBeReduced function
canBeReducedToNormalForm _ = False

isInHeadNormalForm :: Expr Var -> Bool
isInHeadNormalForm = exprIsHNF

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

{-Predicates on Expressions-}

isClassDictionary :: Expr b -> Bool
isClassDictionary (Var name) = "$" `isPrefixOf` varName && not (varName == "$" || varName == "$!") --todo: ugly, find better solution
  where varName = varToString name
isClassDictionary (App expr args) = isClassDictionary (getFunctionOfNestedApplication (App expr args))
isClassDictionary x = False

isPrimitiveTypeConstructorApp :: Expr a -> Bool
isPrimitiveTypeConstructorApp (App (Var var) (Lit literal)) = isPrimitiveTypeConstructorName (varName var)
isPrimitiveTypeConstructorApp _ = False

isPrimitiveTypeConstructorName :: Name -> Bool
isPrimitiveTypeConstructorName name = "#" `isSuffixOf` (nameToString name)

isConstructorApplicationOfType :: Expr a -> String -> Bool
isConstructorApplicationOfType (App expr arg) name = do
  let (function, arguments) = collectArgs (App expr arg)
  case function of
    (Var var) -> (==) (varToString var) name
isConstructorApplicationOfType _ _ = False

isBoolVarTrue :: Var -> Bool
isBoolVarTrue x = (==) (varToSimpleString x) "True"

isBoolVarFalse :: Var -> Bool
isBoolVarFalse x = (==) (varToSimpleString x) "False"

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
      where constructorName = varToString var
isNonEmptyTuple _ = False

isEmptyTuple :: Expr a -> Bool  --can this be checked more elegantly?
isEmptyTuple (Var var) = varToString var == "()"
isEmptyTuple _ = False

isTuple :: Expr a -> Bool
isTuple expr = (||) (isNonEmptyTuple expr) (isEmptyTuple expr)

isListType :: Type -> Bool --is there a more elegant solution?
isListType ty = "[" `isPrefixOf` typeRepresentation && "]" `isSuffixOf` typeRepresentation
  where typeRepresentation = showOutputable ty

isIntType :: Type -> Bool --is there a more elegant solution?
isIntType ty = showOutputable ty == "Int"

isBoolType :: Type -> Bool --is there a more elegant solution?
isBoolType ty = showOutputable ty == "Bool"

isCharType :: Type -> Bool --is there a more elegant solution?
isCharType ty = showOutputable ty == "Char"

isTypeInformation :: Expr b -> Bool
isTypeInformation (Type _) = True
isTypeInformation x = isClassDictionary x

isTypeWrapperFunctionName :: String -> Bool
isTypeWrapperFunctionName name = "#" `isSuffixOf` name

isApplicationWithClassDictionary :: Expr b -> Bool
isApplicationWithClassDictionary expr = do
  let (function, arguments) = collectArgs expr
  length arguments >= 2 && ((isVarExpression function && isTypeInformation (head arguments)) && isClassDictionary (arguments !! 1))

{-Extracting from expressions-}

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
    let values = snd (splitList elements)
    values
  | otherwise = error "expression is not a tuple"

getFunctionOfNestedApplication :: Expr b -> Expr b
getFunctionOfNestedApplication expr = fst (collectArgs expr)

getLiteralArgument  :: Expr a -> Expr a
getLiteralArgument (App (Var var) (Lit literal)) = (Lit literal)
getLiteralArgument _ = error "function expects (App Var Lit)"

extractArgumentsOfNestedApplication :: Expr Var -> [Expr Var]
extractArgumentsOfNestedApplication expr = snd (collectArgs expr)

removeTypeInformation :: [Expr b] -> [Expr b]
removeTypeInformation list = filter (not . isTypeInformation) list