module OriginalCoreAST.CoreStepperHelpers.TracerHelper (module OriginalCoreAST.CoreStepperHelpers.TracerHelper) where

import GHC.Plugins (Expr, Var(..), Type(..), OutputableBndr, varName, varType, isId, isTyVar, isTcTyVar)
import GHC.Core.TyCo.Rep (Type(..))
import Utils (showOutputable)
import Debug.Trace(trace)

traceExpression :: String -> Expr Var -> Expr Var --for debugging
traceExpression comment expr = trace ((comment ++ ": ") ++ (showOutputable expr)) expr

traceMaybeExpression :: String -> Maybe (Expr Var) -> Maybe (Expr Var) --for debugging
traceMaybeExpression comment (Just expr) = trace ((comment ++ ": ") ++ (showOutputable expr)) (Just expr)
traceMaybeExpression comment Nothing = trace ((comment ++ ": ") ++ "Nothing") Nothing

varDescription :: Var -> String
varDescription x = concat ["name: ", showOutputable (varName x), " type: ", showOutputable (varType x), " typeDescription: ", typeDescription (varType x), " isId: ", show (isId x), " isTyVar: ", show (isTyVar x), " isTcTyVar: ", show (isTcTyVar x)]

typeDescription :: Type -> String
typeDescription (TyVarTy _) = "TyVarTy"
typeDescription (AppTy _ _) = "AppTy"
typeDescription (TyConApp _ _) = "TyConApp"
typeDescription (ForAllTy _ wrappedType) = "ForAllTy with wrapped type: " ++ typeDescription wrappedType
typeDescription (FunTy _ _ _ _) = "FunTy"
typeDescription (LitTy _) = "LitTy"
typeDescription (CastTy _ _) = "CastTy"
typeDescription (CoercionTy _) = "CohersionTy"  