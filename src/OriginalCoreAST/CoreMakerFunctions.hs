module OriginalCoreAST.CoreMakerFunctions(fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression, charToCoreLiteral, rationalToCoreLiteral, expressionListToCoreList)
where

import GHC.Core (Expr (..))
import GHC.Types.Literal(Literal (..), mkLitInt64, mkLitString)
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import GHC.Types.Name(nameUnique, Name, mkSystemVarName, mkSysTvName, mkSystemName, pprNameUnqualified, nameStableString, getOccString)
import GHC.Types.Unique (minLocalUnique)
import GHC.Data.FastString (mkFastString)
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))
import GHC.Types.Id.Info ( vanillaIdInfo, IdDetails(..))
import GHC.Types.Name.Occurrence (mkOccName, mkVarOcc)
import GHC.Core.Make(mkCoreTup, mkListExpr, mkFloatExpr, mkDoubleExpr, mkCharExpr, mkNothingExpr, mkJustExpr, mkCoreConApps)
import GHC.Builtin.Types (intTy, anyTy, anyTyCon, anyTypeOfKind)
import GHC.Core.DataCon  ( DataCon, dataConWorkId )
import Data.Maybe

integerToCoreLiteral :: Integer -> Literal
integerToCoreLiteral = mkLitInt64

fractionalToCoreLiteral :: Real a => a -> Literal
fractionalToCoreLiteral value = (LitDouble (toRational value))

charToCoreLiteral :: Char -> Literal
charToCoreLiteral = LitChar

rationalToCoreExpression :: Rational -> Expr b
rationalToCoreExpression value = Lit (rationalToCoreLiteral value)

floatToCoreExpression :: Float -> Expr Var
floatToCoreExpression value = mkFloatExpr value

doubleToCoreExpression :: Double -> Expr Var
doubleToCoreExpression value = mkDoubleExpr value

charToCoreExpression :: Char -> Expr Var
charToCoreExpression value = mkCharExpr value

rationalToCoreLiteral :: Rational -> Literal
rationalToCoreLiteral = LitDouble

maybeToCoreExpression :: Maybe (Expr Var) -> Type -> Expr Var
maybeToCoreExpression element customType =  if isNothing element
                                                then (mkNothingExpr customType)
                                                else (mkJustExpr customType (fromJust element))

integerToCoreExpression :: Integer -> Expr b
integerToCoreExpression value = Lit (integerToCoreLiteral value)

stringToCoreExpression :: String -> Expr b
stringToCoreExpression value = Lit (mkLitString value)

boolToCoreExpression :: Bool -> Expr Var  --this is a hack, we just took the easiest constructors we found to create a "Var"-Instance without understanding what those constructors stand for 
boolToCoreExpression True = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc ("True"))) (LitTy (StrTyLit (mkFastString "Bool"))) vanillaIdInfo)
boolToCoreExpression False = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc ("False"))) (LitTy (StrTyLit (mkFastString "Bool"))) vanillaIdInfo)

expressionListToCoreTuple :: [Expr Var] -> Expr Var
expressionListToCoreTuple expressions = mkCoreTup expressions

expressionListToCoreList :: Type -> [Expr Var] -> Expr Var
expressionListToCoreList listTyle expressions = mkListExpr listTyle expressions

customConstructorapplicationToCoreExpression :: DataCon -> [Expr Var] -> Expr Var
customConstructorapplicationToCoreExpression constructor arguments = mkCoreConApps constructor arguments