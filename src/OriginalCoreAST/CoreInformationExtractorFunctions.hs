module OriginalCoreAST.CoreInformationExtractorFunctions (varExpressionToString, varToString, nameToString, coreLiteralToFractional, isInHeadNormalForm, isTypeInformation, canBeReduced) where

import Data.List (isPrefixOf)
import GHC.Plugins
  ( Expr (Type, Var),
    Literal (LitDouble, LitFloat),
    Name,
    Var (varName),
    exprIsHNF,
    getOccString,
  )

varExpressionToString :: Expr Var -> String
varExpressionToString (Var var) = varToString var
varExpressionToString _ = error "Expression is no var"

varToString :: Var -> String
varToString var = nameToString (varName var)

nameToString :: Name -> String
nameToString = getOccString

coreLiteralToFractional :: Fractional a => Literal -> a
coreLiteralToFractional (LitFloat value) = fromRational value
coreLiteralToFractional (LitDouble value) = fromRational value

isInHeadNormalForm :: Expr Var -> Bool
isInHeadNormalForm = exprIsHNF

isTypeInformation :: Expr Var -> Bool
isTypeInformation (Type _) = True
isTypeInformation (Var name) = "$" `isPrefixOf` varToString name
isTypeInformation x = False

-- | The "canBeReducedFunction" checks if a Core expression can be reduced.
canBeReduced :: Expr Var -> Bool
canBeReduced exp
  | isTypeInformation exp = False
  | isBooleanVar exp = False
  | otherwise = not (exprIsHNF exp)

isBooleanVar :: Expr Var -> Bool
isBooleanVar (Var x) = (==) (varToString x) "True" || (==) (varToString x) "False"
isBooleanVar _ = False