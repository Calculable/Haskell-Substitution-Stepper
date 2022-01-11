{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Compiler
Description : To compile the users Haskell input file into different GHC stages and finally into Haskell Core
License     : GPL-3
-}
module Compiler (compileToCore, writeDump, getCoreProgram) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Map.Strict (Map, fromList, toList)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T
import GHC
  ( DesugaredModule (dm_core_module),
    DynFlags (hscTarget),
    HscTarget (HscNothing),
    LoadHowMuch (LoadAllTargets),
    ParsedModule (pm_parsed_source),
    SuccessFlag (Failed, Succeeded),
    TypecheckedModule (tm_typechecked_source),
    addTarget,
    desugarModule,
    getModSummary,
    getSessionDynFlags,
    guessTarget,
    load,
    mkModuleName,
    parseModule,
    runGhc,
    setSessionDynFlags,
    typecheckModule,
  )
import GHC.Core (CoreProgram)
import GHC.Driver.Types
  ( ModGuts
      ( mg_binds,
        mg_fam_insts,
        mg_insts,
        mg_patsyns,
        mg_rdr_env,
        mg_tcs
      ),
  )
import GHC.Paths (libdir)
import System.Directory
  ( createDirectory,
    doesDirectoryExist,
    removeDirectory,
  )
import System.FilePath (joinPath, takeBaseName)
import Utils (textAst, textOutputable)

data CompilationResult = CompilationResult
  { coreProgram :: CoreProgram,
    debugInfo :: Map Text Text
  }

-- |extracts the Core program from the compilation result
getCoreProgram :: CompilationResult -> CoreProgram
getCoreProgram = coreProgram

-- |takes a file and compiles it to Core
compileToCore :: FilePath -> IO CompilationResult
compileToCore filePath = runGhc (Just libdir) $ do
  dFlags <- getSessionDynFlags
  setSessionDynFlags dFlags {hscTarget = HscNothing}

  target <- guessTarget filePath Nothing
  addTarget target
  res <- load LoadAllTargets
  case res of
    Succeeded -> liftIO $ pure () --liftIO $ putStrLn "successfully loaded targets"
    Failed -> error "failed to load targets" -- liftIO $ putStrLn "failed to load targets"
  let modName = mkModuleName (takeBaseName filePath)
  modSum <- getModSummary modName

  psmod <- parseModule modSum
  tcmod <- typecheckModule psmod
  dsmod <- desugarModule tcmod

  let parserAst = pm_parsed_source psmod
      tcAst = tm_typechecked_source tcmod
      coreModule = dm_core_module dsmod
      coreAst = mg_binds coreModule
      coreReaderEnv = mg_rdr_env coreModule
      coreTyCons = mg_tcs coreModule
      coreClassInsts = mg_insts coreModule
      coreFamInsts = mg_fam_insts coreModule
      corePatternSyns = mg_patsyns coreModule
      debugInformation =
        fromList
          [ ("parserAst.txt", textAst parserAst),
            ("tcAst.txt", textAst tcAst),
            ("coreAST.txt", textAst coreAst),
            ("coreProgram.txt", textOutputable coreAst),
            ("coreReaderEnv.txt", textAst coreReaderEnv),
            ("coreTyCons.txt", textAst coreTyCons),
            ("coreClassInsts.txt", textAst coreClassInsts),
            ("coreFamInsts.txt", textOutputable coreFamInsts),
            ("corePatternSyns.txt", textAst corePatternSyns)
          ]

  return $ CompilationResult coreAst debugInformation

debugDirectoryPath :: String
debugDirectoryPath = "dump"

-- |writes a cuompilation result into a file
writeDump :: CompilationResult -> IO ()
writeDump cr = do
  debugDirExists <- doesDirectoryExist debugDirectoryPath
  if debugDirExists
    then do
      removeDirectory debugDirectoryPath
      createDirectory debugDirectoryPath
    else do
      createDirectory debugDirectoryPath
  mapM_ (\(k, v) -> T.writeFile (joinPath [debugDirectoryPath, unpack k]) v) (toList (debugInfo cr))
