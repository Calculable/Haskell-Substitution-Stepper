-- example for using the parser without ghc monad, not used at the moment
module Parser where

import DynFlags (baseDynFlags)
import GHC (DynFlags, Located, GenLocated (L))
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Parser (parseModule)
import GHC.Parser.Lexer (P (unP), ParseResult (PFailed, POk), mkPState)
import GHC.Plugins (mkFastString, mkRealSrcLoc)
import qualified GHC.Hs

-- https://hackage.haskell.org/package/ghc-lib-parser-9.0.1.20210324/docs/GHC-Parser.html
runParser :: DynFlags -> FilePath -> String -> P a -> ParseResult a
runParser flags filename source parser = unP parser parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer source
    parseState = mkPState flags buffer location

parse :: FilePath -> String -> Located GHC.Hs.HsModule
parse filename source = let
    res = runParser baseDynFlags filename source parseModule
    in case res of
        PFailed pstate -> undefined
        POk pstate result -> result
