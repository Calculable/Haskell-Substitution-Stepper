{-|
Module      : CoreInformationExtractorFunctions
Description : Extracts informations and properties out of Core expressions
License     : GPL-3

This module contains helper functions to extract information inside Core expressions and to 
check Core expressions for properties and conditions.
-}
module OriginalCoreAST.CoreInformationExtractorFunctions (varToString, nameToString, isTypeInformation, canBeReduced, isList, isListType, isEmptyList, isTuple, isVarExpression, isClassDictionary, getFunctionOfNestedApplication, isIntType, isBoolType, isCharType, boolValueFromVar, isBoolVar, removeTypeInformation, getIndividualElementsOfList, getIndividualElementsOfTuple, isPrimitiveTypeConstructorApp, isPrimitiveTypeConstructorName, getLiteralArgument, isTypeWrapperFunctionName, canBeReducedToNormalForm, varRefersToUnsteppableFunction, varsHaveTheSameName, varNameEqualsString, varsHaveTheSameType, isApplicationWithClassDictionary, functionNameMatchesFunctionFromDictionary, convertToMultiArgumentLamda, convertToMultiLet, removeTypeVars, showOperatorWithoutBrackets, isOperator, varToSimpleString, getClassDictionaryVar) where

import Data.List
import GHC.Plugins
import Utils
import GHC.Core.TyCo.Rep
import OriginalCoreAST.CoreTypeDefinitions

unsteppableFunctionPrefix = "unsteppableFunction'"

{-Var Functions-}

-- |converts a Var into a printable String
varToString :: Var -> String
varToString var =
  if isBoolVar (Var var) --find better solution for boolean workaround
    then varToSimpleString var
    else showOutputable var

-- |checks if an expression is of Type "Var"
isVarExpression :: Expr b -> Bool
isVarExpression (Var name) = True
isVarExpression _ = False

-- |checks if an expression represents a boolean value
isBoolVar :: Expr b -> Bool
isBoolVar (Var x) = isBoolVarTrue x || isBoolVarFalse x
isBoolVar _ = False

-- |converts an expression which represents a boolean into the boolean value
boolValueFromVar :: Var -> Bool
boolValueFromVar x | isBoolVarTrue x = True
                   | isBoolVarFalse x = False
                   | otherwise = error "the provided variable does not represent a bool" 

-- |converts a Var into a printable String. The output of this function
-- is sometimes shorter than the output of "showOutputable". For 
-- example where showOutputable would result in the string "True_XO" this 
-- function would result in the string "True"
varToSimpleString :: Var -> String
varToSimpleString var = nameToString (varName var)

-- |provides the string representation of a name
nameToString :: Name -> String
nameToString = getOccString

-- |checks if a var refers to a function that is unsteppable. If a function is 
-- prefixed with "unsteppableFunction'" it means that the function should be
-- evaluated with the stepper backend instead of parsing the function body
varRefersToUnsteppableFunction :: Var -> Bool
varRefersToUnsteppableFunction name = unsteppableFunctionPrefix `isPrefixOf` varToString name

-- compares two vars and checks if they have the same string representation
varsHaveTheSameName :: Var -> Var -> Bool 
varsHaveTheSameName x y = (==) (varToString x) (varToString y)

-- compares two vars and checks if they have the same type representation
varsHaveTheSameType :: Var -> Var -> Bool 
varsHaveTheSameType x y = (==) (showOutputable (varType x)) (showOutputable (varType y))

-- checks if a var has a specific string representation
varNameEqualsString :: Var -> String -> Bool
varNameEqualsString var name = (==) (varToString var) name

{-Reduction functions-}

-- | checks if a Core expression is not yet in normal form and can further be reduced
canBeReducedToNormalForm :: CoreExpr -> Bool
--canBeReducedToNormalForm (Lam b expr) = canBeReducedToNormalForm expr 
canBeReducedToNormalForm (App expr argument) = do
  let (function, arguments) = collectArgs (App expr argument)
  (any canBeReduced arguments || any canBeReducedToNormalForm arguments) || canBeReduced function || canBeReducedToNormalForm function 
canBeReducedToNormalForm expr = canBeReduced expr

-- | checks if a Core expression is not yet in head normal form and can further be reduced
canBeReduced :: CoreExpr -> Bool
canBeReduced exp
  | isBoolVar exp = False
  | isTypeWrapperVar exp = True
  | isTypeInformation exp = False
  | otherwise = case exp of
    (App (Lam _ _) x) -> True
    (App (Let _ _) x) -> True
    (Case {}) -> True
    (Cast _ _) -> True
    (Let _ _) -> True
    (App x y) -> canBeReduced (getFunctionOfNestedApplication (App x y)) || not (isInHeadNormalForm exp)
    _ -> not (isInHeadNormalForm exp)
    where
      isInHeadNormalForm :: CoreExpr -> Bool
      isInHeadNormalForm = exprIsHNF

      isTypeWrapperVar :: Expr b -> Bool
      isTypeWrapperVar (Var var) = isTypeWrapperFunctionName (varToString var)
      isTypeWrapperVar _ = False

{-Predicates on Expressions-}

-- | checks if an expression represents a class dictionary containing the class functions for a specific type.
-- for example a Var "$EqInteger" would be a class dictionary
isClassDictionary :: Expr b -> Bool
isClassDictionary (Var name) = "$" `isPrefixOf` varName && not (varName == "$" || varName == "$!") --todo: ugly, find better solution
  where varName = varToString name
isClassDictionary (App expr args) = isClassDictionary (getFunctionOfNestedApplication (App expr args))
isClassDictionary x = False

-- |unwrapps var from an expression. Precondition: expression is a var
getVar :: Expr b -> Var
getVar (Var var) = var
getVar _ = error "expression is not a var"

-- |checks if an expression is an application of a primitive type constructor. For example 
-- "I# 3" or "C# 'a'" would be such primitive type constructor applications
isPrimitiveTypeConstructorApp :: Expr b -> Bool
isPrimitiveTypeConstructorApp (App (Var var) (Lit literal)) = isPrimitiveTypeConstructorName (varName var)
isPrimitiveTypeConstructorApp _ = False

-- |checks if a function name stands for a primitive type constructor like "I#" or "C#". 
isPrimitiveTypeConstructorName :: Name -> Bool
isPrimitiveTypeConstructorName name = isTypeWrapperFunctionName (nameToString name)

-- |checks if a function name stands for a primitive type constructor like "I#" or "C#". 
isTypeWrapperFunctionName :: String -> Bool
isTypeWrapperFunctionName name = "#" `isSuffixOf` name

-- |checks if a (nested) application is the instanciation of a type with a specific constructor name, provided as String
isConstructorApplicationOfType :: Expr b -> String -> Bool
isConstructorApplicationOfType (App expr arg) name = do
  let (function, arguments) = collectArgs (App expr arg)
  case function of
    (Var var) -> (==) (varToString var) name
    _ -> False
isConstructorApplicationOfType _ _ = False

-- |checks if a Var represents the boolean value "True"
isBoolVarTrue :: Var -> Bool
isBoolVarTrue x = (==) (varToSimpleString x) "True"

-- |checks if a Var represents the boolean value "False"
isBoolVarFalse :: Var -> Bool
isBoolVarFalse x = (==) (varToSimpleString x) "False"

-- |checks if a Core expression represents a non-empty list
isNonEmptyList :: Expr b -> Bool --can this be checked more elegantly
isNonEmptyList (App expr arg) = isConstructorApplicationOfType (App expr arg) ":" && isList arg
isNonEmptyList _ = False

-- |checks if a Core expression represents an empty list
isEmptyList :: Expr b -> Bool
isEmptyList expr = isConstructorApplicationOfType expr "[]"

-- |checks if a Core expression represents a list
isList :: Expr b -> Bool
isList expr = (||) (isNonEmptyList expr) (isEmptyList expr)

-- |checks if a Core expression represents a non-empty tuple
isNonEmptyTuple :: Expr b -> Bool --can this be checked more elegantly?
isNonEmptyTuple (App expr arg) = do
  let (function, arguments) = collectArgs (App expr arg)
  case function of
    (Var var) -> (("(" `isPrefixOf` constructorName) && (")" `isSuffixOf` constructorName)) && (',' `elem` constructorName)
      where constructorName = varToString var
    _ -> False
isNonEmptyTuple _ = False

-- |checks if a Core expression represents a empty tuple
isEmptyTuple :: Expr b -> Bool  --can this be checked more elegantly?
isEmptyTuple (Var var) = varToString var == "()"
isEmptyTuple _ = False

-- |checks if a Core expression represents a tuple
isTuple :: Expr b -> Bool
isTuple expr = (||) (isNonEmptyTuple expr) (isEmptyTuple expr)

-- |checks if a Core type describes a list
isListType :: Type -> Bool --is there a more elegant solution?
isListType ty = "[" `isPrefixOf` typeRepresentation && "]" `isSuffixOf` typeRepresentation
  where typeRepresentation = showOutputable ty

-- |checks if a Core type describes an Int
isIntType :: Type -> Bool --is there a more elegant solution?
isIntType ty = showOutputable ty == "Int"

-- |checks if a Core type describes a Bool
isBoolType :: Type -> Bool --is there a more elegant solution?
isBoolType ty = showOutputable ty == "Bool"

-- |checks if a Core type describes a Char
isCharType :: Type -> Bool --is there a more elegant solution?
isCharType ty = showOutputable ty == "Char"

-- |checks if a Core expression contains type information (either is a "Type" expression or a reference to a type class dictionary)
isTypeInformation :: Expr b -> Bool
isTypeInformation (Type _) = True
isTypeInformation x = isClassDictionary x

-- |checks if an expression is an application of a function defined inside a class dictionary
-- for example, this would be the case for the expression "== @Integer @Integer $fEqInteger 1 1"
-- and it would not be the case for the expression "anyFunction 1 1"
isApplicationWithClassDictionary :: Expr b -> Bool
isApplicationWithClassDictionary expr = do
  let (function, arguments) = collectArgs expr
  length arguments >= 2 && ((isVarExpression function && isTypeInformation (head arguments)) && isClassDictionary (arguments !! 1))

-- |checks if a name of a function matches a name of a function that is inside a class dictionary
functionNameMatchesFunctionFromDictionary :: FunctionReference -> Expr Var -> Bool
functionNameMatchesFunctionFromDictionary searchFunctionName (Var dictionaryFunctionName) = varToString searchFunctionName `isSuffixOf` varToString dictionaryFunctionName
functionNameMatchesFunctionFromDictionary searchFunctionName (App expr args) = functionNameMatchesFunctionFromDictionary searchFunctionName (getFunctionOfNestedApplication (App expr args))
functionNameMatchesFunctionFromDictionary _ _ = False

{-Extracting from expressions-}

-- |takes a list represented as a Core expression and returns all the elements as a "real" list 
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
  | otherwise = [expr]

-- |takes a tuple represented as a Core expression and returns all the elements as a list
getIndividualElementsOfTuple :: Expr b -> [Expr b]
getIndividualElementsOfTuple expr
  | isEmptyTuple expr = []
  | isTuple expr = do
    let (constructor, elements) = collectArgs expr
    let values = snd (splitList elements)
    values
  | otherwise = error "expression is not a tuple"

-- |extracts the var (function binding) of a class dictionary from an expression
getClassDictionaryVar :: Expr b -> Var
getClassDictionaryVar (Var var) = var
getClassDictionaryVar (App expr arg) = getClassDictionaryVar expr
getClassDictionaryVar _ = error "type dictionary has unexpected type"

-- |extracts the left-most innermost function of a nested function application
getFunctionOfNestedApplication :: Expr b -> Expr b
getFunctionOfNestedApplication expr = fst (collectArgs expr)

-- |extracts the literal value out of an application with one single literal argument
getLiteralArgument  :: Expr b -> Expr b
getLiteralArgument (App (Var var) (Lit literal)) = (Lit literal)
getLiteralArgument _ = error "function expects (App Var Lit)"

-- |takes a nested application in Core and returns all the arguments as a one dimensional list
extractArgumentsOfNestedApplication :: Expr b -> [Expr b]
extractArgumentsOfNestedApplication expr = snd (collectArgs expr)

-- |takes a nested lamda expression and returns a list of lamda bindings with an expression
convertToMultiArgumentLamda :: Expr b -> ([b], Expr b)
convertToMultiArgumentLamda (Lam bind expr) = do
  let (nestedBindings, nestedExpression) = convertToMultiArgumentLamda expr
  (bind:nestedBindings, nestedExpression)
convertToMultiArgumentLamda expr = ([], expr)  

-- |takes a nested let expression and returns a list of bindings with an expression
convertToMultiLet :: Expr b -> ([(b, Expr b)], Expr b)
convertToMultiLet (Let (NonRec b bindingExpression) expr) = do
  let (nestedBindings, nestedExpression) = convertToMultiLet expr
  ((b, bindingExpression):nestedBindings, nestedExpression)
convertToMultiLet (Let (Rec bindings) expr) = do
  let (nestedBindings, nestedExpression) = convertToMultiLet expr
  (bindings ++ nestedBindings, nestedExpression)
convertToMultiLet expr = ([], expr)  

-- |takes a list of expressions and returns all expressions that are not type information. 
-- this is useful for function evaluation where the type information is not needed
removeTypeInformation :: [Expr b] -> [Expr b]
removeTypeInformation list = filter (not . isTypeInformation) list

-- |takes a list of vars and returns all vars that are not type information. 
removeTypeVars :: [Var] -> [Var]
removeTypeVars list = filter (not . isTyVar) list

-- |checks if an expression is an operator function
-- operator functions would be (+) (-) and other functions with brackets
isOperator :: CoreExpr -> Bool
isOperator (Var var) = ("(" `isPrefixOf` expressionString) && (")" `isSuffixOf` expressionString)
  where expressionString = showOutputable (Var var :: CoreExpr)
isOperator _ = False

-- |gives the printable representation for an operator without brackets
showOperatorWithoutBrackets :: CoreExpr -> String
showOperatorWithoutBrackets (Var var) = varToString var
showOperatorWithoutBrackets _ = error "is not an operator"