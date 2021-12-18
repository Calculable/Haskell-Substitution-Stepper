module DataProvider.DataProvider (getBindingFinderWithCoreBindings) where

import Compiler
import GHC.Plugins
import OriginalCoreAST.CoreStepperHelpers.CoreLookup
import OriginalCoreAST.CoreStepperPrinter
import OriginalCoreAST.CoreTypeDefinitions

getBindingFinderWithCoreBindings :: IO (String -> CoreExpr, [Binding])
getBindingFinderWithCoreBindings = do
  finder <- bindingFinder
  bindings <- coreBindings
  return ((`findBindingForString` bindings), bindings)

  where
    findBinding :: FunctionName -> IO CoreExpr
    findBinding name = do findBindingForString name <$> coreBindings

    bindingFinder :: IO (FunctionName -> CoreExpr)
    bindingFinder = do
      bindings <- coreBindings
      return (`findBindingForString` bindings)    

coreBindings = do
  program <- coreProgram
  prelude <- corePrelude
  return (convertToBindingsList program ++ convertToBindingsList prelude)      

  where
    coreProgram :: IO CoreProgram
    coreProgram = do getCoreProgram <$> compiledCore

    corePrelude :: IO CoreProgram
    corePrelude = do getCoreProgram <$> compiledPrelude

    compiledCore = compileToCore "src/IntegrationTestBindings.hs"
    compiledPrelude = compileToCore "src/SteppablePrelude.hs"