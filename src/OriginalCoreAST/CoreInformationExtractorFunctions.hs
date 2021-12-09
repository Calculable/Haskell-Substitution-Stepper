module OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced)
where

import GHC.Core (Expr (..))
import GHC.Types.Literal(Literal (..), mkLitInt64, mkLitString)
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import GHC.Types.Name(nameUnique, Name, mkSystemVarName, mkSysTvName, mkSystemName, pprNameUnqualified, nameStableString, getOccString)
import GHC.Core.Utils (exprIsHNF)
import Data.List(isPrefixOf)
import Debug.Trace(trace)
import Utils (showOutputable)

varExpressionToString :: Expr Var -> String
varExpressionToString (Var var) = varToString var
varExpressionToString _ = error "Expression is no var"

varToString :: Var -> String
varToString var = 
  if (isBooleanVar (Var var)) --find better solution for boolean workaround
    then varToSimpleString var
    else (showOutputable var)


coreLiteralToFractional :: Fractional a => Literal -> a
coreLiteralToFractional (LitFloat value) = fromRational value
coreLiteralToFractional (LitDouble value) = fromRational value

isInHeadNormalForm :: Expr Var -> Bool
isInHeadNormalForm = exprIsHNF

isTypeInformation :: Expr Var -> Bool
isTypeInformation (Type _) = True
isTypeInformation (Var name) = "$" `isPrefixOf` (varToString name)
isTypeInformation (App expr arg) =  isTypeInformation expr
isTypeInformation x = False

-- | The "canBeReducedFunction" checks if a Core expression can be reduced.
canBeReduced exp
  | isTypeInformation exp = False
  | isBooleanVar exp = False
  | otherwise = not (exprIsHNF exp)

isBooleanVar :: Expr Var -> Bool
isBooleanVar (Var x) = or [((==) (varToSimpleString x) "True"), ((==) (varToSimpleString x) "False")]
isBooleanVar _ = False

varToSimpleString :: Var -> String
varToSimpleString var = nameToString (varName var) 

nameToString :: Name -> String
nameToString = getOccString