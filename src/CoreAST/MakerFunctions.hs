{-|
Module      : CoreMakerFunctions
Description : Helps to create instances of Core expressions
License     : GPL-3

This module contains helper functions to create Core expressions and Core literals.
-}
module CoreAST.MakerFunctions (fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression, charToCoreLiteral, rationalToCoreLiteral, expressionListToCoreList, expressionTupleToCoreTuple, maybeToCoreExpression, expressionListToCoreListWithType, charToCoreExpression) where

import GHC.Core.TyCo.Rep
import GHC.Plugins
import GHC.Types.Unique

-- |creates a Core literal for an integer value
integerToCoreLiteral :: Integer -> Literal
integerToCoreLiteral = mkLitInt64

-- |creates a Core literal for a fractional/real value
fractionalToCoreLiteral :: Real a => a -> Literal
fractionalToCoreLiteral value = LitDouble (toRational value)

-- |creates a Core literal for a char value
charToCoreLiteral :: Char -> Literal
charToCoreLiteral = LitChar

-- |creates a Core literal for a rational value
rationalToCoreLiteral :: Rational -> Literal
rationalToCoreLiteral = LitDouble

-- |creates a Core expression for a rational value
rationalToCoreExpression :: Rational -> Expr b
rationalToCoreExpression value = Lit (rationalToCoreLiteral value)

-- |creates a Core expression for a float value
floatToCoreExpression :: Float -> CoreExpr
floatToCoreExpression = mkFloatExpr

-- |creates a Core expression for a double value
doubleToCoreExpression :: Double -> CoreExpr
doubleToCoreExpression = mkDoubleExpr

-- |creates a Core expression for a char value
charToCoreExpression :: Char -> CoreExpr
charToCoreExpression = mkCharExpr

-- |creates a Core expression that represents a maybe type
maybeToCoreExpression :: Maybe CoreExpr -> Type -> CoreExpr
maybeToCoreExpression element customType = maybe (mkNothingExpr customType) (mkJustExpr customType) element

-- |creates a Core expression for a integer value
integerToCoreExpression :: Integer -> Expr b
integerToCoreExpression value = Lit (integerToCoreLiteral value)

-- |creates a Core expression for a string value
stringToCoreExpression :: String -> Expr b
stringToCoreExpression value = Lit (mkLitString value)

-- |creates a Core expression for a bool value
boolToCoreExpression :: Bool -> CoreExpr --this is a hack, we just took the easiest constructors we found to create a "Var"-Instance without understanding what those constructors stand for
boolToCoreExpression True = makeSimpleConstructorWithContructorNameAndTypeName "True" "Bool"
boolToCoreExpression False = makeSimpleConstructorWithContructorNameAndTypeName "False" "Bool"

-- |creates a Core expression for a given constructor and type name
makeSimpleConstructorWithContructorNameAndTypeName :: String -> String -> CoreExpr --this is a hack, we just took the easiest constructors we found to create a "Var"-Instance without understanding what those constructors stand for
makeSimpleConstructorWithContructorNameAndTypeName constructorName typeName = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc constructorName)) (LitTy (StrTyLit (mkFastString typeName))) vanillaIdInfo)

-- |converts a tuple of Core Expressions into a Core Expression that represents a tuple
expressionTupleToCoreTuple :: (CoreExpr, CoreExpr) -> CoreExpr
expressionTupleToCoreTuple (first, second) = expressionListToCoreTuple [first, second]
  where
    expressionListToCoreTuple :: [CoreExpr] -> CoreExpr
    expressionListToCoreTuple = mkCoreTup    

-- |converts a list of Core Expressions into a Core Expression which represents a list
expressionListToCoreListWithType :: Type -> [CoreExpr] -> CoreExpr
expressionListToCoreListWithType = mkListExpr

-- |converts a list of Core Expressions into a Core Expression which represents a list
-- This function only works for lists containing literal values. Otherwise the function 
-- "expressionListToCoreListWithType" has to be used
expressionListToCoreList :: [CoreExpr] -> Maybe CoreExpr
expressionListToCoreList (Lit (LitChar x) : xs) = Just $ expressionListToCoreListWithType charTy (Lit (LitChar x) : xs)
expressionListToCoreList (Lit (LitNumber x y) : xs) = Just $ expressionListToCoreListWithType intTy (Lit (LitNumber x y) : xs)
expressionListToCoreList (Lit (LitString x) : xs) = Just $ expressionListToCoreListWithType stringTy (Lit (LitString x) : xs)
expressionListToCoreList (Lit (LitFloat x) : xs) = Just $ expressionListToCoreListWithType floatTy (Lit (LitFloat x) : xs)
expressionListToCoreList (Lit (LitDouble x) : xs) = Just $ expressionListToCoreListWithType doubleTy (Lit (LitDouble x) : xs)
expressionListToCoreList _ = trace "this type cannot be converted to list" Nothing

-- |creates a Core Expression that represents arguments applied to a data constructor
customConstructorapplicationToCoreExpression :: DataCon -> [CoreExpr] -> CoreExpr
customConstructorapplicationToCoreExpression = mkCoreConApps