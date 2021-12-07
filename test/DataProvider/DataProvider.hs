module DataProvider.DataProvider(exampleExpression) where

import Compiler (compileToCore, writeDump, getCoreProgram)
import OriginalCoreAST.CoreStepperPrinter
  (convertToBindingsList)
import OriginalCoreAST.CoreStepperHelpers.CoreLookup(findBindingForString)
import GHC.Core (Bind (NonRec), CoreProgram, Expr)
import GHC.Types.Var (Var (varName, varType))
import GHC.Plugins (liftIO)

exampleExpression :: IO (Expr Var)
exampleExpression = findBinding "a"

findBinding :: String -> IO (Expr Var)
findBinding name = do findBindingForString name <$> coreBindings

compiledCore = compileToCore "src/TestBindings.hs"

coreProgram = do getCoreProgram <$> compiledCore

coreBindings = do convertToBindingsList <$> coreProgram