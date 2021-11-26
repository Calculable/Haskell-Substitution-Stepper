module SimplifiedCoreAST.SimplifiedCoreASTConverter
  (simplifyBindings, simplifyLiteral, isTypeInformation
  )
where

import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt, AltCon (..), CoreBind)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString),
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id)
import GHC.Utils.Outputable (Outputable (ppr), OutputableBndr)
import Utils (showOutputable)
import GHC.Core.Ppr
  ( pprCoreAlt,
  )
import Data.List(isPrefixOf)
import SimplifiedCoreAST.SimplifiedCoreAST (ExpressionS(..), LiteralS(..), AltS(..), AltConS(..), BindS(..))
import GHC.Types.Name(nameUnique, Name)

simplifyBindings :: [CoreBind] -> [BindS]
simplifyBindings x = concat (map simplifyBinding x)

simplifyBinding :: CoreBind -> [BindS]
simplifyBinding (NonRec b exp) = [simplifyBindingWithExpression (b, exp)]
simplifyBinding (Rec bindings) = map simplifyBindingWithExpression bindings

simplifyBindingWithExpression :: (Var, Expr Var) -> BindS
simplifyBindingWithExpression (b, exp)  = ((varToString b), (simplifyExpression exp))

simplifyExpression :: Expr Var -> ExpressionS
simplifyExpression (Var id) = VarS (varToString id)
simplifyExpression (Lit lit) = LitS (simplifyLiteral lit)
simplifyExpression (App exp arg) = 
  if (isTypeInformation simplifiedArgument) then simplifiedExpression
  else if (isUnnecessaryFunction simplifiedExpression) then simplifiedArgument
  else AppS simplifiedExpression simplifiedArgument
    where simplifiedArgument = (simplifyExpression arg)
          simplifiedExpression = (simplifyExpression exp)
simplifyExpression (Lam b exp) = LamS (varToString b) (simplifyExpression exp)
simplifyExpression (Type t) = TypeS
simplifyExpression (Case exp b t alts) = CaseS (simplifyExpression exp) (map simplifyAlt alts)
simplifyExpression (Let bind exp) = InvalidExpression "Let Constructor not suppoted"
simplifyExpression (Cast _ _) = InvalidExpression "Cast Constructor not suppoted"
simplifyExpression (Tick _ _) = InvalidExpression "Tick Constructor not suppoted"
simplifyExpression (Coercion _) = InvalidExpression "Coercion Constructor not suppoted"

simplifyLiteral :: Literal -> LiteralS
simplifyLiteral (LitChar c) =  (LitCharS c)
simplifyLiteral (LitNumber t v) =  (LitNumberS v)
simplifyLiteral (LitString bs) =  (LitStringS (show bs))
simplifyLiteral (LitFloat f) =  (LitFloatS f)
simplifyLiteral (LitDouble d) =  (LitDoubleS d)

simplifyAlt :: Alt Var -> AltS
simplifyAlt (altCon, b, exp) =  ((simplifyAltCon altCon), (map varToString b), (simplifyExpression exp))

simplifyAltCon :: AltCon -> AltConS
simplifyAltCon (DataAlt constructor) = DataAltS (showOutputable constructor)
simplifyAltCon (LitAlt literal) = LitAltS (simplifyLiteral literal)
simplifyAltCon (DEFAULT) = DefaultS

isTypeInformation :: ExpressionS -> Bool
isTypeInformation (TypeS) = True
isTypeInformation (VarS name) = isPrefixOf "$" name
isTypeInformation x = False

isUnnecessaryFunction :: ExpressionS -> Bool
isUnnecessaryFunction (VarS name) = (==) "unpackCString#" name 
isUnnecessaryFunction x = False

varToString :: Var -> String
varToString var = nameToString (varName var)

nameToString :: Name -> String
nameToString name = showOutputable name