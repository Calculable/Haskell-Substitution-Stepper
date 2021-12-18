module OriginalCoreAST.CoreStepperHelpers.CoreTracerHelper where

import GHC.Plugins
import GHC.Core.TyCo.Rep
import Utils
import Debug.Trace

traceExpression :: String -> CoreExpr -> CoreExpr
traceExpression comment expr = trace ((comment ++ ": ") ++ showOutputable expr) expr

traceMaybeExpression :: String -> Maybe CoreExpr -> Maybe CoreExpr
traceMaybeExpression comment (Just expr) = trace ((comment ++ ": ") ++ showOutputable expr) (Just expr)
traceMaybeExpression comment Nothing = trace ((comment ++ ": ") ++ "Nothing") Nothing

varDescription :: Var -> String
varDescription x = do
  concat ["name: ", showOutputable (varName x), " type: ", showOutputable (varType x), " typeDescription: ", typeDescription (varType x), " isId: ", show (isId x), " isTyVar: ", show (isTyVar x), " isTcTyVar: ", show (isTcTyVar x)]
    where
      typeDescription :: Type -> String
      typeDescription (TyVarTy _) = "TyVarTy"
      typeDescription (AppTy _ _) = "AppTy"
      typeDescription (TyConApp _ _) = "TyConApp"
      typeDescription (ForAllTy _ wrappedType) = "ForAllTy with wrapped type: " ++ typeDescription wrappedType
      typeDescription (FunTy {}) = "FunTy"
      typeDescription (LitTy _) = "LitTy"
      typeDescription (CastTy _ _) = "CastTy"
      typeDescription (CoercionTy _) = "CohersionTy"  

typeOfExpression :: Expr b -> String --used for tracing / debugging
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