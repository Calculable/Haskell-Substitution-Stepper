module OriginalCoreAST.CorePrettyPrinter (prettyPrint, prettyPrintToString) where

import GHC.Plugins (Expr, OutputableBndr, Var)
import Utils (showOutputable)

prettyPrint :: Expr Var -> IO ()
prettyPrint exp = putStr (prettyPrintToString exp)

prettyPrintToString :: OutputableBndr b => Expr b -> String
prettyPrintToString = showOutputable
