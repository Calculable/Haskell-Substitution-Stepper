module SimplifiedCoreAST.SimplifiedCoreASTConverter
  ( simplifyBindings,
    simplifyLiteral,
    isTypeInformation,
  )
where

import Data.List (isPrefixOf)
import GHC.Core (Alt, AltCon (..), Bind (NonRec, Rec), Expr (..))
import GHC.Core.Ppr
  ( pprCoreAlt,
  )
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString),
  )
import GHC.Types.Var (Id, TyVar, Var (varName, varType))
import GHC.Utils.Outputable (Outputable (ppr), OutputableBndr)
import SimplifiedCoreAST.SimplifiedCoreAST (AltConS (..), AltS (..), BindS (..), ExpressionS (..), LiteralS (..))
import Utils (printAst, showOutputable)

simplifyBindings :: (OutputableBndr a) => [Bind a] -> [BindS]
simplifyBindings x = concat (map simplifyBinding x)

simplifyBinding :: (OutputableBndr a) => Bind a -> [BindS]
simplifyBinding (NonRec b exp) = [simplifyBindingWithExpression (b, exp)]
simplifyBinding (Rec bindings) = map simplifyBindingWithExpression bindings

simplifyBindingWithExpression :: (OutputableBndr b) => (b, Expr b) -> BindS
simplifyBindingWithExpression (b, exp) = (showOutputable b, simplifyExpression exp)

simplifyExpression :: (OutputableBndr b) => Expr b -> ExpressionS
simplifyExpression (Var id) = VarS (showOutputable $ varName id)
simplifyExpression (Lit lit) = LitS (simplifyLiteral lit)
simplifyExpression (App exp arg)
  | isTypeInformation simplifiedArgument = simplifiedExpression
  | isUnnecessaryFunction simplifiedExpression = simplifiedArgument
  | otherwise = AppS simplifiedExpression simplifiedArgument
  where
      simplifiedArgument = simplifyExpression arg
      simplifiedExpression = simplifyExpression exp
simplifyExpression (Lam b exp) = LamS (showOutputable b) (simplifyExpression exp)
simplifyExpression (Type t) = TypeS
simplifyExpression (Case exp b t alts) = CaseS (simplifyExpression exp) (map simplifyAlt alts)
simplifyExpression (Let bind exp) = InvalidExpression "Let Constructor not suppoted"
simplifyExpression (Cast _ _) = InvalidExpression "Cast Constructor not suppoted"
simplifyExpression (Tick _ _) = InvalidExpression "Tick Constructor not suppoted"
simplifyExpression (Coercion _) = InvalidExpression "Coercion Constructor not suppoted"

simplifyLiteral :: Literal -> LiteralS
simplifyLiteral (LitChar c) = LitCharS c
simplifyLiteral (LitNumber t v) = LitNumberS v
simplifyLiteral (LitString bs) = LitStringS (show bs)
simplifyLiteral (LitFloat f) = LitFloatS f
simplifyLiteral (LitDouble d) = LitDoubleS d

simplifyAlt :: (OutputableBndr b) => Alt b -> AltS
simplifyAlt (altCon, b, exp) = (simplifyAltCon altCon, map showOutputable b, simplifyExpression exp)

simplifyAltCon :: AltCon -> AltConS
simplifyAltCon (DataAlt constructor) = DataAltS (showOutputable constructor)
simplifyAltCon (LitAlt literal) = LitAltS (simplifyLiteral literal)
simplifyAltCon DEFAULT = DefaultS

isTypeInformation :: ExpressionS -> Bool
isTypeInformation TypeS = True
isTypeInformation (VarS name) = "$" `isPrefixOf` name
isTypeInformation x = False

isUnnecessaryFunction :: ExpressionS -> Bool
isUnnecessaryFunction (VarS name) = (==) "unpackCString#" name
isUnnecessaryFunction x = False