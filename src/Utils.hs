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

showOutputable :: Outputable a => a -> String
showOutputable = showSDoc baseDynFlags . ppr

textOutputable :: Outputable a => a -> Text
textOutputable = pack . showOutputable

printAst :: Data a => a -> String
printAst = showOutputable . showAstData BlankSrcSpan

textAst :: Data a => a -> Text
textAst = pack . printAst

printCore :: (MonadIO m, OutputableBndr b) => [Bind b] -> m ()
printCore coreAst = liftIO (putStrLn (showOutputable (pprCoreBindings coreAst)))

listTopLevelFunctions :: CoreProgram -> IO ()
listTopLevelFunctions cp = do
  let topLevelNames = map (takeWhile (/= ' ') . showOutputable . pprCoreBinding) cp
  mapM_ putStrLn topLevelNames

(??) :: Maybe a -> a -> a
(??) = flip fromMaybe