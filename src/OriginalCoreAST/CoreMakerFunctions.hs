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
import GHC.Builtin.Types (intTy, anyTy, anyTyCon, anyTypeOfKind, charTy, stringTy, floatTy, doubleTy)
import GHC.Core.DataCon  ( DataCon, dataConWorkId )
import Data.Maybe
import Debug.Trace(trace)

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

expressionListToCoreListWithType :: Type -> [Expr Var] -> Expr Var
expressionListToCoreListWithType listTyle expressions = mkListExpr listTyle expressions

expressionListToCoreList :: [Expr Var] -> Maybe (Expr Var)
expressionListToCoreList (Lit (LitChar x):xs) = Just $ expressionListToCoreListWithType charTy (Lit (LitChar x):xs)
expressionListToCoreList (Lit (LitNumber x y):xs) = Just $ expressionListToCoreListWithType intTy (Lit (LitNumber x y):xs) 
expressionListToCoreList (Lit (LitString x):xs) = Just $ expressionListToCoreListWithType stringTy (Lit (LitString x):xs)
expressionListToCoreList (Lit (LitFloat x):xs) = Just $ expressionListToCoreListWithType floatTy (Lit (LitFloat x):xs)
expressionListToCoreList (Lit (LitDouble x):xs) = Just $ expressionListToCoreListWithType doubleTy (Lit (LitDouble x):xs)
expressionListToCoreList _ = trace "this type cannot be converted to list" Nothing


customConstructorapplicationToCoreExpression :: DataCon -> [Expr Var] -> Expr Var
customConstructorapplicationToCoreExpression constructor arguments = mkCoreConApps constructor arguments