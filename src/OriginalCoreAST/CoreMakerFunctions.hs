module OriginalCoreAST.CoreMakerFunctions (fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression, charToCoreLiteral, rationalToCoreLiteral) where

import GHC.Core.TyCo.Rep (TyLit (StrTyLit), Type (LitTy))
import GHC.Plugins
  ( Expr (Lit, Var),
    IdDetails (VanillaId),
    Literal (LitChar, LitDouble),
    Var,
    mkFastString,
    mkGlobalVar,
    mkLitInt64,
    mkLitString,
    mkSystemName,
    mkVarOcc,
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

rationalToCoreLiteral :: Rational -> Literal
rationalToCoreLiteral = LitDouble

integerToCoreExpression :: Integer -> Expr b
integerToCoreExpression value = Lit (integerToCoreLiteral value)

stringToCoreExpression :: String -> Expr Var
stringToCoreExpression value = Lit (mkLitString value)

boolToCoreExpression :: Bool -> Expr Var --this is a hack, we just took the easiest constructors we found to create a "Var"-Instance without understanding what those constructors stand for
boolToCoreExpression True = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc "True")) (LitTy (StrTyLit (mkFastString "Bool"))) vanillaIdInfo)
boolToCoreExpression False = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc "False")) (LitTy (StrTyLit (mkFastString "Bool"))) vanillaIdInfo)
