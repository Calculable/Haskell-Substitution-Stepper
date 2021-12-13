module OriginalCoreAST.CoreMakerFunctions (fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression, charToCoreLiteral, rationalToCoreLiteral, expressionListToCoreList, expressionTupleToCoreTuple, maybeToCoreExpression, expressionListToCoreListWithType) where

import GHC.Core.TyCo.Rep (TyLit (StrTyLit), Type (LitTy))
import GHC.Plugins
  ( DataCon,
    Expr (Lit, Var),
    IdDetails (VanillaId),
    Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString),
    Var,
    charTy,
    doubleTy,
    floatTy,
    intTy,
    mkCharExpr,
    mkCoreConApps,
    mkCoreTup,
    mkDoubleExpr,
    mkFastString,
    mkFloatExpr,
    mkGlobalVar,
    mkJustExpr,
    mkListExpr,
    mkLitInt64,
    mkLitString,
    mkNothingExpr,
    mkSystemName,
    mkVarOcc,
    stringTy,
    trace,
    vanillaIdInfo,
  )
import GHC.Types.Unique (minLocalUnique)

integerToCoreLiteral :: Integer -> Literal
integerToCoreLiteral = mkLitInt64

fractionalToCoreLiteral :: Real a => a -> Literal
fractionalToCoreLiteral value = LitDouble (toRational value)

charToCoreLiteral :: Char -> Literal
charToCoreLiteral = LitChar

rationalToCoreExpression :: Rational -> Expr b
rationalToCoreExpression value = Lit (rationalToCoreLiteral value)

floatToCoreExpression :: Float -> Expr Var
floatToCoreExpression = mkFloatExpr

doubleToCoreExpression :: Double -> Expr Var
doubleToCoreExpression = mkDoubleExpr

charToCoreExpression :: Char -> Expr Var
charToCoreExpression = mkCharExpr

rationalToCoreLiteral :: Rational -> Literal
rationalToCoreLiteral = LitDouble

maybeToCoreExpression :: Maybe (Expr Var) -> Type -> Expr Var
maybeToCoreExpression element customType = maybe (mkNothingExpr customType) (mkJustExpr customType) element

integerToCoreExpression :: Integer -> Expr b
integerToCoreExpression value = Lit (integerToCoreLiteral value)

stringToCoreExpression :: String -> Expr b
stringToCoreExpression value = Lit (mkLitString value)

boolToCoreExpression :: Bool -> Expr Var --this is a hack, we just took the easiest constructors we found to create a "Var"-Instance without understanding what those constructors stand for
boolToCoreExpression True = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc "True")) (LitTy (StrTyLit (mkFastString "Bool"))) vanillaIdInfo)
boolToCoreExpression False = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc "False")) (LitTy (StrTyLit (mkFastString "Bool"))) vanillaIdInfo)

expressionListToCoreTuple :: [Expr Var] -> Expr Var
expressionListToCoreTuple = mkCoreTup

expressionTupleToCoreTuple :: (Expr Var, Expr Var) -> Expr Var
expressionTupleToCoreTuple (first, second) = expressionListToCoreTuple [first, second]

expressionListToCoreListWithType :: Type -> [Expr Var] -> Expr Var
expressionListToCoreListWithType = mkListExpr

expressionListToCoreList :: [Expr Var] -> Maybe (Expr Var)
expressionListToCoreList (Lit (LitChar x) : xs) = Just $ expressionListToCoreListWithType charTy (Lit (LitChar x) : xs)
expressionListToCoreList (Lit (LitNumber x y) : xs) = Just $ expressionListToCoreListWithType intTy (Lit (LitNumber x y) : xs)
expressionListToCoreList (Lit (LitString x) : xs) = Just $ expressionListToCoreListWithType stringTy (Lit (LitString x) : xs)
expressionListToCoreList (Lit (LitFloat x) : xs) = Just $ expressionListToCoreListWithType floatTy (Lit (LitFloat x) : xs)
expressionListToCoreList (Lit (LitDouble x) : xs) = Just $ expressionListToCoreListWithType doubleTy (Lit (LitDouble x) : xs)
expressionListToCoreList _ = trace "this type cannot be converted to list" Nothing

customConstructorapplicationToCoreExpression :: DataCon -> [Expr Var] -> Expr Var
customConstructorapplicationToCoreExpression = mkCoreConApps