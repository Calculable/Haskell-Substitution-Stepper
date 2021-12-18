module OriginalCoreAST.CorePrettyPrinter (prettyPrint) where

import GHC.Plugins
import Utils
import Data.List

prettyPrint :: OutputableBndr b => Expr b -> IO ()
prettyPrint exp = putStr (prettyPrintToString exp)
  where
    prettyPrintToString :: OutputableBndr b => Expr b -> String
    prettyPrintToString = showOutputable

printFlatCoreBinding :: (OutputableBndr a) => Bind a -> IO ()
printFlatCoreBinding (NonRec b exp) = printFlatBindingWithExpression (b, exp)
printFlatCoreBinding (Rec bindings) = mapM_ printFlatBindingWithExpression bindings

printFlatBindingWithExpression :: (OutputableBndr b) => (b, Expr b) -> IO ()
printFlatBindingWithExpression (b, exp) = do
  putStr (showOutputable b)
  putStr " = "
  printFlatCoreExpression exp
  putStrLn ""      

printFlatCoreExpression :: (OutputableBndr b) => Expr b -> IO ()
printFlatCoreExpression (Var id) = do
  let name = showOutputable $ varName id
  if "$" `isPrefixOf` name
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
  putStr (showOutputable (ppr t))
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
  where
    printFlatAlt :: (OutputableBndr b) => Alt b -> IO ()
    printFlatAlt (altCon, b, exp) = do
      putStr "\n\t"
      putStr (showOutputable altCon) --either literal or constructor or _
      putStr (unwords (map showOutputable b))
      putStr " -> "
      printFlatCoreExpression exp
printFlatCoreExpression x = do
  putStr "Unsupported Constructor"
