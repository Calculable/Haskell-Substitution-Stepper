module OriginalCoreAST.CoreInformationExtractorFunctions(showVarExpression, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced)
where

import GHC.Core (Expr (..))
import GHC.Types.Literal(Literal (..), mkLitInt64, mkLitString)
import GHC.Types.Var (Var (varName, varType), TyVar, Id, mkCoVar, mkGlobalVar)
import GHC.Types.Name(nameUnique, Name, mkSystemVarName, mkSysTvName, mkSystemName, pprNameUnqualified, nameStableString, getOccString)
import GHC.Core.Utils (exprIsHNF)
import Data.List(isPrefixOf)


showVarExpression :: Expr Var -> String
showVarExpression (Var var) = varToString var
showVarExpression _ = error "Expression is no var"


varToString :: Var -> String
varToString var = nameToString (varName var)

nameToString :: Name -> String
nameToString name = getOccString name


coreLiteralToFractional :: Fractional a => Literal -> a
coreLiteralToFractional (LitFloat value) = fromRational value
coreLiteralToFractional (LitDouble value) = fromRational value

isInHeadNormalForm :: Expr Var -> Bool
isInHeadNormalForm exp = exprIsHNF exp

isTypeInformation :: Expr Var -> Bool
isTypeInformation (Type _) = True
isTypeInformation (Var name) = "$" `isPrefixOf` (varToString name)
isTypeInformation x = False

canBeReduced exp = if isTypeInformation exp
                    then False
                    else 
                        if isBooleanVar exp --hack for booleans created by ourself. maybe replace with boolean from the prelude later
                            then False
                            else not (exprIsHNF exp)

isBooleanVar :: Expr Var -> Bool
isBooleanVar (Var x) = or [((==) (varToString x) "True"), ((==) (varToString x) "False")]
isBooleanVar _ = False