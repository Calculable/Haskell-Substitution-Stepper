module DataProvider.DataProvider (getBindingFinderWithCoreBindings) where

import Compiler (compileToCore, getCoreProgram)
import GHC.Plugins (CoreProgram, Expr, Var)
import OriginalCoreAST.CoreStepperHelpers.CoreLookup
  ( findBindingForString,
  )
import OriginalCoreAST.CoreStepperPrinter (convertToBindingsList)

findBinding :: String -> IO (Expr Var)
findBinding name = do findBindingForString name <$> coreBindings

bindingFinder :: IO (String -> Expr Var)
bindingFinder = do
  bindings <- coreBindings
  return (\x -> findBindingForString x bindings)

compiledCore = compileToCore "src/IntegrationTestBindings.hs"

coreProgram :: IO CoreProgram
coreProgram = do getCoreProgram <$> compiledCore

coreBindings = do convertToBindingsList <$> coreProgram

getBindingFinderWithCoreBindings :: IO (String -> Expr Var, [(Var, Expr Var)])
getBindingFinderWithCoreBindings = do
  finder <- bindingFinder
  bindings <- coreBindings
  return (\x -> findBindingForString x bindings, bindings)
