{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main where

import qualified Data.Data
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Session
import GHC.Fingerprint
import qualified GHC.Hs
import GHC.Hs.Dump
import GHC.Parser
import GHC.Parser.Lexer
import GHC.Platform
import GHC.Settings
import GHC.Settings.Config
import GHC.Types.SrcLoc
import GHC.Utils.Outputable

main :: IO ()
main = do
  source <- readFile "src/Source.hs"
  let parsed = parse source
  putStrLn $ printParsed parsed

-- parse a string of haskell source code using the 'parseModule' parser from ghc-lib-parser
parse :: String -> ParseResult (Located GHC.Hs.HsModule)
parse source = runParser baseDynFlags source parseModule

-- showAstData: https://hackage.haskell.org/package/ghc-lib-parser-9.0.1.20210324/docs/GHC-Hs-Dump.html
-- showSDocDebug, showSDocDump: https://hackage.haskell.org/package/ghc-lib-parser-9.0.1.20210324/docs/GHC-Utils-Outputable.html#g:3
printParsed :: Data.Data.Data a => ParseResult a -> String
printParsed parsed = case parsed of
  POk s a -> showSDocDump baseDynFlags $ showAstData BlankSrcSpan a
  PFailed s -> undefined

-- https://hackage.haskell.org/package/ghc-lib-parser-9.0.1.20210324/docs/GHC-Parser.html
runParser :: DynFlags -> String -> P a -> ParseResult a
runParser flags str parser = unP parser parseState
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

-- https://github.com/tweag/ormolu/blob/a320d995833c1d07ae73324f66ff27154861244b/src/GHC/DynFlags.hs
baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

fakeSettings :: Settings
fakeSettings =
  Settings
    { sGhcNameVersion =
        GhcNameVersion
          { ghcNameVersion_programName = "ghc",
            ghcNameVersion_projectVersion = cProjectVersion
          },
      sFileSettings = FileSettings {},
      sTargetPlatform =
        Platform
          { platformWordSize = PW8,
            platformMini =
              PlatformMini
                { platformMini_arch = ArchUnknown,
                  platformMini_os = OSUnknown
                },
            platformUnregisterised = True,
            platformByteOrder = LittleEndian,
            platformHasGnuNonexecStack = False,
            platformHasIdentDirective = False,
            platformHasSubsectionsViaSymbols = False,
            platformIsCrossCompiling = False,
            platformLeadingUnderscore = False,
            platformTablesNextToCode = False
          },
      sPlatformMisc = PlatformMisc {},
      sPlatformConstants =
        PlatformConstants {pc_DYNAMIC_BY_DEFAULT = False, pc_WORD_SIZE = 8},
      sToolSettings =
        ToolSettings
          { toolSettings_opt_P_fingerprint = fingerprint0,
            toolSettings_pgm_F = ""
          }
    }

fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []
