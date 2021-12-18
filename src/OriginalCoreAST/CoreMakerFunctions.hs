module OriginalCoreAST.CoreMakerFunctions (fractionalToCoreLiteral, integerToCoreLiteral, rationalToCoreExpression, integerToCoreExpression, stringToCoreExpression, boolToCoreExpression, charToCoreLiteral, rationalToCoreLiteral, expressionListToCoreList, expressionTupleToCoreTuple, maybeToCoreExpression, expressionListToCoreListWithType, charToCoreExpression) where

import GHC.Core.TyCo.Rep
import GHC.Plugins
import GHC.Types.Unique

integerToCoreLiteral :: Integer -> Literal
integerToCoreLiteral = mkLitInt64

fractionalToCoreLiteral :: Real a => a -> Literal
fractionalToCoreLiteral value = LitDouble (toRational value)

charToCoreLiteral :: Char -> Literal
charToCoreLiteral = LitChar

rationalToCoreLiteral :: Rational -> Literal
rationalToCoreLiteral = LitDouble

rationalToCoreExpression :: Rational -> Expr b
rationalToCoreExpression value = Lit (rationalToCoreLiteral value)

floatToCoreExpression :: Float -> CoreExpr
floatToCoreExpression = mkFloatExpr

doubleToCoreExpression :: Double -> CoreExpr
doubleToCoreExpression = mkDoubleExpr

charToCoreExpression :: Char -> CoreExpr
charToCoreExpression = mkCharExpr

maybeToCoreExpression :: Maybe CoreExpr -> Type -> CoreExpr
maybeToCoreExpression element customType = maybe (mkNothingExpr customType) (mkJustExpr customType) element

integerToCoreExpression :: Integer -> Expr b
integerToCoreExpression value = Lit (integerToCoreLiteral value)

stringToCoreExpression :: String -> Expr b
stringToCoreExpression value = Lit (mkLitString value)

boolToCoreExpression :: Bool -> CoreExpr --this is a hack, we just took the easiest constructors we found to create a "Var"-Instance without understanding what those constructors stand for
boolToCoreExpression True = makeSimpleConstructorWithContructorNameAndTypeName "True" "Bool"
boolToCoreExpression False = makeSimpleConstructorWithContructorNameAndTypeName "False" "Bool"

makeSimpleConstructorWithContructorNameAndTypeName :: String -> String -> CoreExpr --this is a hack, we just took the easiest constructors we found to create a "Var"-Instance without understanding what those constructors stand for
makeSimpleConstructorWithContructorNameAndTypeName constructorName typeName = Var (mkGlobalVar VanillaId (mkSystemName minLocalUnique (mkVarOcc constructorName)) (LitTy (StrTyLit (mkFastString typeName))) vanillaIdInfo)

expressionListToCoreTuple :: [CoreExpr] -> CoreExpr
expressionListToCoreTuple = mkCoreTup

expressionTupleToCoreTuple :: (CoreExpr, CoreExpr) -> CoreExpr
expressionTupleToCoreTuple (first, second) = expressionListToCoreTuple [first, second]

expressionListToCoreListWithType :: Type -> [CoreExpr] -> CoreExpr
expressionListToCoreListWithType = mkListExpr

expressionListToCoreList :: [CoreExpr] -> Maybe CoreExpr
expressionListToCoreList (Lit (LitChar x) : xs) = Just $ expressionListToCoreListWithType charTy (Lit (LitChar x) : xs)
expressionListToCoreList (Lit (LitNumber x y) : xs) = Just $ expressionListToCoreListWithType intTy (Lit (LitNumber x y) : xs)
expressionListToCoreList (Lit (LitString x) : xs) = Just $ expressionListToCoreListWithType stringTy (Lit (LitString x) : xs)
expressionListToCoreList (Lit (LitFloat x) : xs) = Just $ expressionListToCoreListWithType floatTy (Lit (LitFloat x) : xs)
expressionListToCoreList (Lit (LitDouble x) : xs) = Just $ expressionListToCoreListWithType doubleTy (Lit (LitDouble x) : xs)
expressionListToCoreList _ = trace "this type cannot be converted to list" Nothing

customConstructorapplicationToCoreExpression :: DataCon -> [CoreExpr] -> CoreExpr
customConstructorapplicationToCoreExpression = mkCoreConApps