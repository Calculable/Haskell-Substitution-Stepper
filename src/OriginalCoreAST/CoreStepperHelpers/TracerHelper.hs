module OriginalCoreAST.CoreStepperHelpers.TracerHelper (module OriginalCoreAST.CoreStepperHelpers.TracerHelper) where

import GHC.Plugins (Expr, Var, OutputableBndr)
import Utils (showOutputable)
import Debug.Trace(trace)

traceExpression :: String -> Expr Var -> Expr Var --for debugging
traceExpression comment expr = trace ((comment ++ ": ") ++ (showOutputable expr)) expr

traceMaybeExpression :: String -> Maybe (Expr Var) -> Maybe (Expr Var) --for debugging
traceMaybeExpression comment (Just expr) = trace ((comment ++ ": ") ++ (showOutputable expr)) (Just expr)
traceMaybeExpression comment Nothing = trace ((comment ++ ": ") ++ "Nothing") Nothing
