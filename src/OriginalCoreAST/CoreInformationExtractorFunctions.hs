module OriginalCoreAST.CoreInformationExtractorFunctions(varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced, isList, isMaybe, isNothingMaybe, isJustMaybe, isListType)
where

import GHC.Core (Expr (..), collectArgs)
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

-- | The "canBeReducedFunction" checks if a Core expression is not yet in head normal form and can further be reduced
canBeReduced exp
  | isTypeInformation exp = False
  | isBooleanVar exp = False
  | otherwise = case exp of {
      (App (Lam _ _) x) -> True;
      (App (Let _ _) x) -> True;
      (Case _ _ _ _) -> True;
      (Let _ _) -> True;
      _ -> not (exprIsHNF exp)
  }

isBooleanVar :: Expr Var -> Bool
isBooleanVar (Var x) = or [((==) (varToSimpleString x) "True"), ((==) (varToSimpleString x) "False")]
isBooleanVar _ = False

varToSimpleString :: Var -> String
varToSimpleString var = nameToString (varName var) 

nameToString :: Name -> String
nameToString = getOccString

isNonEmptyList :: Expr a -> Bool --can this be checked more elegant?
isNonEmptyList expr = isConstructorApplicationOfType expr ":"

isEmptyList :: Expr a -> Bool
isEmptyList expr = isConstructorApplicationOfType expr "[]"

isList :: Expr a -> Bool
isList expr = (||) (isNonEmptyList expr) (isEmptyList expr)

isNothingMaybe :: Expr a -> Bool --can this be checked more elegant?
isNothingMaybe expr = isConstructorApplicationOfType expr "Nothing"

isJustMaybe :: Expr a -> Bool
isJustMaybe expr = isConstructorApplicationOfType expr "Just"

isMaybe :: Expr a -> Bool
isMaybe expr = (||) (isNothingMaybe expr) (isJustMaybe expr)

isConstructorApplicationOfType :: Expr a -> String -> Bool 
isConstructorApplicationOfType (App expr arg) name = do
  let (function, arguments) = collectArgs (App expr arg)
  case function of {
    (Var var) -> (==) (varToString var) name
  } 
isConstructorApplicationOfType _ _ = False 

isListType :: Expr a -> Bool --is there a more elegant solution?
isListType (Type ty) = ((showOutputable ty) == "[]")