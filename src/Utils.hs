-- https://github.com/tweag/ormolu/blob/897e6736ec17d7e359091abe3c24f4abfec015c4/src/Ormolu/Utils.hs

module Utils where

import DynFlags (baseDynFlags)
import qualified GHC.Utils.Outputable as GHC
import Data.Data ( Data )
import GHC.Hs.Dump (showAstData, BlankSrcSpan (BlankSrcSpan))

showOutputable :: GHC.Outputable o => o -> String
showOutputable = GHC.showSDoc baseDynFlags . GHC.ppr

-- https://hackage.haskell.org/package/ghc-lib-parser-9.0.1.20210324/docs/GHC-Hs-Dump.html
dumpAst :: Data a => a -> String
dumpAst x = showOutputable $ showAstData BlankSrcSpan x
