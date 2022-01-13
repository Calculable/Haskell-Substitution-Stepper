-- |
-- Module      : DataProvider
-- Description : Provides a "binding finder" which can be used for integratoin test
-- License     : GPL-3
--
-- The binding finder is a function that can be used to lookup bindings from
-- the IntegrationTestBindings and the SteppablePrelude. It is used during
-- the integration tests to optain Haskell Core expressions to work with.
module DataProvider.DataProvider (getBindingFinderWithCoreBindings) where

import Compiler (compileToCore, getCoreProgram)
import CoreAST.Helpers.Lookup (findExpressionForString)
import CoreAST.StepperPrinter (convertToBindingsList)
import CoreAST.TypeDefinitions (Binding, FunctionName)
import GHC.Plugins (CoreExpr, CoreProgram)

-- | Provides a function that returns a CoreExpression for a given (function) name
--  this function is used inside test cases to easily optain individual
--  Core expressions defined in the File "IntegrationTestBindings"
getBindingFinderWithCoreBindings :: String -> IO (String -> CoreExpr, [Binding])
getBindingFinderWithCoreBindings filename = do
  finder <- bindingFinder
  bindings <- coreBindings
  return ((`findExpressionForString` bindings), bindings)
  where
    findBinding :: FunctionName -> IO CoreExpr
    findBinding name = do findExpressionForString name <$> coreBindings

    bindingFinder :: IO (FunctionName -> CoreExpr)
    bindingFinder = do
      bindings <- coreBindings
      return (`findExpressionForString` bindings)

    -- the list of bindings inside the file "IntegrationTestBindings".
    -- the list also contains bindings defined in the "Steppable Prelude"
    -- (represented as Core Bindings)
    coreBindings = do
      program <- coreProgram
      prelude <- corePrelude
      return (convertToBindingsList program ++ convertToBindingsList prelude)
      where
        coreProgram :: IO CoreProgram
        coreProgram = do getCoreProgram <$> compiledCore

        corePrelude :: IO CoreProgram
        corePrelude = do getCoreProgram <$> compiledPrelude

        compiledCore = compileToCore filename
        compiledPrelude = compileToCore "src/SteppablePrelude.hs"