module OriginalCoreAST.CorePrettyPrinter(prettyPrint, prettyPrintToString)
where

import GHC.Core (Expr (..))
import Utils (showOutputable)
import GHC.Types.Var (Var)
import GHC.Utils.Outputable(OutputableBndr(..))

prettyPrint :: Expr Var -> IO()
prettyPrint exp = putStr (prettyPrintToString exp)

prettyPrintToString :: GHC.Utils.Outputable.OutputableBndr b => Expr b -> String
prettyPrintToString = showOutputable
