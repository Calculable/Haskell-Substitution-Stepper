{-|
Module      : CoreTracerHelper
Description : Helper functions for debugging and tracing
License     : GPL-3

This module contains useful functions to investigate bugs 
and trace expressions.

Note: Tracing statements should not be used in the final version. 
Tracing statements have side-effects (printing) but do not have the 
"IO" type signature and are thus not pure
-}
module CoreAST.Helpers.TraceHelper where

import GHC.Plugins
    ( trace,
      Var(varName, varType),
      CoreExpr,
      Type,
      Expr(..),
      isId,
      isTcTyVar,
      isTyVar )
import GHC.Core.TyCo.Rep
    ( Type(CoercionTy, TyVarTy, AppTy, TyConApp, ForAllTy, FunTy,
           LitTy, CastTy) )
import Utils ( showOutputable )
import Debug.Trace ( trace )

-- |traces an expression together with a comment string
traceExpression :: String -> CoreExpr -> CoreExpr
traceExpression comment expr = trace ((comment ++ ": ") ++ showOutputable expr) expr

-- |traces a maybe expression together with a comment string
traceMaybeExpression :: String -> Maybe CoreExpr -> Maybe CoreExpr
traceMaybeExpression comment (Just expr) = trace ((comment ++ ": ") ++ showOutputable expr) (Just expr)
traceMaybeExpression comment Nothing = trace ((comment ++ ": ") ++ "Nothing") Nothing

-- |provides information about a Var as a printable string. 
-- This function is used for debugging.
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

-- |provides information about a Type as a printable string. 
-- This function is used for debugging.
typeOfExpression :: Expr b -> String 
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