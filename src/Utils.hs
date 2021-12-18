
{-|
Module      : Utils
Description : Contains a bunch of useful functions used by the stepper
License     : GPL-3
-}
module Utils where

import Control.Monad.IO.Class (MonadIO)
import Data.Data (Data)
import Data.Text (Text, pack)
import DynFlags (baseDynFlags)
import GHC.Core.Ppr (pprCoreBinding, pprCoreBindings)
import GHC.Hs.Dump (BlankSrcSpan (BlankSrcSpan), showAstData)
import GHC.Plugins
  ( Bind,
    CoreProgram,
    Outputable (ppr),
    OutputableBndr,
    liftIO,
    showSDoc,
  )
import Data.Maybe

-- |pretty prints the provided argument
showOutputable :: Outputable a => a -> String
showOutputable = showSDoc baseDynFlags . ppr

-- |returns the provided argument as a printable text
textOutputable :: Outputable a => a -> Text
textOutputable = pack . showOutputable

-- |pretty prints a nested abstract syntax tree
printAst :: Data a => a -> String
printAst = showOutputable . showAstData BlankSrcSpan

-- |returns a nested abstract syntax tree as printable text
textAst :: Data a => a -> Text
textAst = pack . printAst

-- |pretty prints a list of Core bindings
printCore :: (MonadIO m, OutputableBndr b) => [Bind b] -> m ()
printCore coreAst = liftIO (putStrLn (showOutputable (pprCoreBindings coreAst)))

-- |takes a core program and pretty prints a list of all top level function bindings contained in this programm
listTopLevelFunctions :: CoreProgram -> IO ()
listTopLevelFunctions cp = do
  let topLevelNames = map (takeWhile (/= ' ') . showOutputable . pprCoreBinding) cp
  mapM_ putStrLn topLevelNames

-- |definition of the ?? opreator (known from javascript)
(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

-- #splits a list in two halves
-- this function is taken from: https://stackoverflow.com/questions/19074520/how-to-split-a-list-into-two-in-haskell
splitList :: [a] -> ([a], [a])
splitList myList = splitAt (((length myList) + 1) `div` 2) myList