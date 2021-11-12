module FlatCoreASTPrinter
  (printFlatCoreAST
  )
where

import GHC.Core (Bind (NonRec, Rec), Expr (..))
import GHC.Types.Literal
  ( Literal (LitChar, LitDouble, LitFloat, LitNumber, LitString),
  )
import GHC.Types.Var (Var (varName, varType))
import GHC.Utils.Outputable (Outputable (ppr), OutputableBndr)
import Utils (printAst, showOutputable)


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
printFlatCoreExpression (Var id) =
    putStr (showOutputable (varName id))
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
  putStr ") "
  printFlatCoreExpression arg
printFlatCoreExpression (Lam b exp) = do
  putStr "\\"
  putStr (showOutputable b)
  putStr " -> ("
  printFlatCoreExpression exp
  putStr ")"
printFlatCoreExpression (Type t) = do
  putStr (showOutputable (ppr t))
printFlatCoreExpression x = do
  putStr "Unsupported Constructor"
