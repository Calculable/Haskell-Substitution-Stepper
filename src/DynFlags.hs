{-# OPTIONS_GHC -Wno-missing-fields #-}

-- |
-- Module      : Compiler
-- Description : basic dynflags for use with the standalone parser
-- License     : GPL-3
--
-- Source and description: https://github.com/tweag/ormolu/blob/f55ab70bff8aabc8f57f3c51035df7e48af5ae25/src/GHC/DynFlags.hs
module DynFlags
  ( baseDynFlags,
  )
where

import GHC.Driver.Session
  ( DynFlags,
    FileSettings (FileSettings),
    GhcNameVersion
      ( GhcNameVersion,
        ghcNameVersion_programName,
        ghcNameVersion_projectVersion
      ),
    LlvmConfig (LlvmConfig),
    PlatformConstants
      ( PlatformConstants,
        pc_DYNAMIC_BY_DEFAULT,
        pc_WORD_SIZE
      ),
    PlatformMisc (PlatformMisc),
    Settings
      ( Settings,
        sFileSettings,
        sGhcNameVersion,
        sPlatformConstants,
        sPlatformMisc,
        sTargetPlatform,
        sToolSettings
      ),
    defaultDynFlags,
  )
import GHC.Fingerprint (fingerprint0)
import GHC.Platform (Arch (ArchUnknown), ByteOrder (LittleEndian), OS (OSUnknown), PlatformWordSize (PW8))
import GHC.Settings
  ( Platform
      ( Platform,
        platformByteOrder,
        platformHasGnuNonexecStack,
        platformHasIdentDirective,
        platformHasSubsectionsViaSymbols,
        platformIsCrossCompiling,
        platformLeadingUnderscore,
        platformMini,
        platformTablesNextToCode,
        platformUnregisterised,
        platformWordSize
      ),
    PlatformMini (PlatformMini, platformMini_arch, platformMini_os),
    ToolSettings (ToolSettings, toolSettings_opt_P_fingerprint, toolSettings_pgm_F),
  )
import GHC.Version (cProjectVersion)

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

baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings fakeLlvmConfig
