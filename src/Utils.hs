module Utils where
import GHC.Plugins ( Outputable(ppr), showSDoc, text )
import Data.Data ( Data )
import DynFlags ( baseDynFlags )
import GHC.Hs.Dump ( showAstData, BlankSrcSpan(BlankSrcSpan) )
import Data.Text (Text, pack)

showOutputable :: Outputable a => a -> String
showOutputable = showSDoc baseDynFlags . ppr

textOutputable :: Outputable a => a -> Text
textOutputable = pack . showOutputable

printAst :: Data a => a -> String
printAst = showOutputable . showAstData  BlankSrcSpan

textAst :: Data a => a -> Text
textAst = pack . printAst
