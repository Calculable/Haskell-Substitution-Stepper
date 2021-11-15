module FlatCoreASTPrinter
  (printFlatCoreAST
  )
where

import GHC.Core (Bind (NonRec, Rec), Expr (..), Alt)
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString),
  )
import GHC.Types.Var (Var (varName, varType), TyVar, Id)
import GHC.Utils.Outputable (Outputable (ppr), OutputableBndr)
import Utils (printAst, showOutputable)
import GHC.Core.Ppr
  ( pprCoreAlt,
  )
import Data.List(isPrefixOf)

printFlatCoreAST :: (OutputableBndr a) => [Bind a] -> IO ()
printFlatCoreAST x = mapM_ printFlatCoreBinding x

printFlatCoreBinding :: (OutputableBndr a) => Bind a -> IO ()
printFlatCoreBinding (NonRec b exp) = printFlatBindingWithExpression (b, exp)
printFlatCoreBinding (Rec bindings) = mapM_ printFlatBindingWithExpression bindings

printFlatBindingWithExpression ::  (OutputableBndr b) => (b, Expr b) -> IO ()
printFlatBindingWithExpression (b, exp) = do
    putStr (showOutputable b)
    putStr " = "
    printFlatCoreExpression exp
    putStrLn ""

printFlatCoreExpression :: (OutputableBndr b) => Expr b -> IO ()
printFlatCoreExpression (Var id) = do
    let name = (showOutputable $ varName id)
    if (isPrefixOf "$" name)
      then putStr ""
      else putStr name
printFlatCoreExpression (Lit lit) = do
    case lit of
      LitChar c -> putStr (show c)
      LitNumber t v -> putStr (show v)
      LitString bs -> putStr (show bs)
      LitFloat f -> putStr (show f)
      LitDouble d -> putStr (show d)
printFlatCoreExpression (App exp arg) = do
  putStr "("
  printFlatCoreExpression exp
  putStr ")"
  printFlatCoreExpression arg
printFlatCoreExpression (Lam b exp) = do
  putStr "\\"
  putStr (showOutputable b)
  putStr " -> ("
  printFlatCoreExpression exp
  putStr ")"
printFlatCoreExpression (Type t) = do
  --putStr (showOutputable (ppr t))
  putStr ""
printFlatCoreExpression (Let bind exp) = do
  printFlatCoreBinding bind
  printFlatCoreExpression exp
printFlatCoreExpression (Case exp b t alts) = do
  putStr "case "
  printFlatCoreExpression exp
  --putStr "(b)"
  --putStr (showOutputable b)
  --putStr "(t)"
  --putStr (showOutputable (ppr t))
  putStr " of {"
  mapM_ printFlatAlt alts
  putStr "};"
printFlatCoreExpression x = do
  putStr "Unsupported Constructor"


printFlatAlt :: (OutputableBndr b) => Alt b -> IO ()
printFlatAlt (altCon, b, exp) = do
    putStr "\n\t"
    putStr (showOutputable altCon) --either literal or constructor or _
    putStr (unwords ( (map showOutputable b)))
    putStr " -> "
    printFlatCoreExpression exp

data ExpressionS
    = VarS String --for example "x" or "+"
    | LitS LiteralS --for example "4"
    | AppS {expressionS:: ExpressionS, argumentS :: ExpressionS} --for example "+ 1"
    | LamS {parameterS:: String, expressionS:: ExpressionS} --for example "\x -> ...""
    | CaseS {expressionS:: ExpressionS, alternativesS:: [AltS]} --for example "\a -> case (== a 1) of {true -> "One" false -> "not one"};"
    | InvalidExpression String --for example "unsupported expression"

data LiteralS
    = LitCharS Char
    | LitNumberS Integer
    | LitStringS String
    | LitFloatS Rational
    | LitDoubleS Rational

type AltS = (AltConS, [String], ExpressionS)

data AltConS
    = DataAltS String --pattern is a constructor, for example ":"
    | LitAltS  LiteralS -- pattern is a literal, for example "TRUE"
    | DefaultS -- pattern is "_"