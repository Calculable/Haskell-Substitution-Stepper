module DataProvider.DataProvider (getBindingFinderWithCoreBindings) where

import Compiler
import GHC.Plugins
import OriginalCoreAST.CoreStepperHelpers.CoreLookup
import OriginalCoreAST.CoreStepperPrinter

findBinding :: String -> IO (Expr Var)
findBinding name = do findBindingForString name <$> coreBindings

bindingFinder :: IO (String -> Expr Var)
bindingFinder = do
  bindings <- coreBindings
  return (`findBindingForString` bindings)

compiledCore = compileToCore "src/IntegrationTestBindings.hs"
compiledPrelude = compileToCore "src/SteppablePrelude.hs"

coreProgram :: IO CoreProgram
coreProgram = do getCoreProgram <$> compiledCore

corePrelude :: IO CoreProgram
corePrelude = do getCoreProgram <$> compiledPrelude

coreBindings = do
  program <- coreProgram
  prelude <- corePrelude
  return (convertToBindingsList program ++ convertToBindingsList prelude)

getBindingFinderWithCoreBindings :: IO (String -> Expr Var, [(Var, Expr Var)])
getBindingFinderWithCoreBindings = do
  finder <- bindingFinder
  bindings <- coreBindings
  return ((`findBindingForString` bindings), bindings)
