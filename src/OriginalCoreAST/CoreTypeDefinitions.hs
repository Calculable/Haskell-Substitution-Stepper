{-|
Module      : CoreTypeDefinitions
Description : Contains Type aliases
License     : GPL-3
-}
module OriginalCoreAST.CoreTypeDefinitions where
import GHC.Plugins

type Binding = (Var, Expr Var)
type Reducer = (Expr Var -> Maybe (Expr Var))
type StepResult = (ReductionStepDescription, Expr Var, [Binding])
type ReductionStepDescription = String --for example: "replace x with definition"
type Argument = Expr Var
type Function = Expr Var
type FunctionReference = Var
type FunctionName = String