{-|
Module      : CoreTypeDefinitions
Description : Contains Type aliases
License     : GPL-3
-}
module OriginalCoreAST.CoreTypeDefinitions where
import GHC.Plugins
import Utils
import Data.List

type Binding = (Var, Expr Var)
type Reducer = (Expr Var -> Maybe (Expr Var))
type StepResult = (ReductionStepDescription, Expr Var, [Binding])
type Argument = Expr Var
type Function = Expr Var
type FunctionReference = Var
type FunctionName = String

data ReductionStepDescription
  = DeltaReductionStep FunctionReference
  | ApplicationStep CoreExpr
  | EvaluationStep FunctionReference
  | CaseExpressionStep
  | PatternMatchStep
  | ReplaceLetStep FunctionReference
  | RemoveCohersionStep
  | ApplicationExpressionStep
  | ClassDictionaryLookupStep FunctionReference FunctionReference
  | StrictApplicationArgumentStep --not real Core behaviour but used for unsteppable functions
  | NestedReduction [ReductionStepDescription]

instance Show ReductionStepDescription where
  show (DeltaReductionStep var) = "Replace '" ++ showOutputable var ++ "' with definition"
  show (ApplicationStep _) = "Application"
  show (EvaluationStep var) = "Evaluate unsteppable function/operator " ++ showOutputable var
  show CaseExpressionStep = "Reduce Case Expression"
  show PatternMatchStep = "Replace with matching pattern"
  show (ReplaceLetStep var) = "Replace binding '" ++ showOutputable var ++ "' with definition"
  show RemoveCohersionStep = "Remove Cohersion part from expression"
  show ApplicationExpressionStep = "Reduce function of application"
  show (ClassDictionaryLookupStep function classDictionary) = "Replace '" ++ showOutputable function ++ "' with definition from class dictionary '" ++ showOutputable classDictionary ++ "'"
  show StrictApplicationArgumentStep = "Reduce application argument. (Note: this is done for unsteppable functions only - non-standard strict behaviour!)"
  show (NestedReduction descriptions) = intercalate " -> " (map show descriptions)
