{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Cli
-- Description : Methods for the Cli-Frontend of the stepper
-- License     : GPL-3
module Cli (runCli, dispatch) where

import Compiler (compileToCore, getCoreProgram, writeDump)
import CoreAST.StepperPrinter
  ( printCoreStepByStepReductionForEveryBinding,
  )
import CoreAST.TypeDefinitions ()
import Data.Maybe (fromMaybe)
import Options.Generic
  ( Generic,
    Modifiers (shortNameModifier),
    ParseRecord (..),
    Text,
    Unwrapped,
    Wrapped,
    defaultModifiers,
    firstLetter,
    parseRecordWithModifiers,
    unwrapRecord,
    type (:::),
    type (<?>),
  )
import Utils (listTopLevelFunctions, printCore)
import Prelude hiding (FilePath)
import qualified Prelude as P (FilePath)
import System.FilePath (takeDirectory, combine, takeFileName)
import System.Directory ( setCurrentDirectory, doesFileExist )

type FilePath = P.FilePath <?> "The Haskell source file used as input to substep"

type FunctionIdentifier = Maybe String <?> "Top level function to step through"

type VerbosityLevel = Maybe Integer <?> "Verbosity level between 1 and 4"

type ShowComments = Maybe Bool <?> "Show text comments describing each reduction step"

subStepDescription :: Text
subStepDescription = "The Haskell Substitution Stepper"

data SubStep w
  = Step
      { path :: w ::: FilePath,
        function :: w ::: FunctionIdentifier,
        verbose :: w ::: VerbosityLevel,
        comments :: w ::: ShowComments
      }
  | Print
      { path :: w ::: FilePath,
        function :: w ::: FunctionIdentifier
      }
  | List
      { path :: w ::: FilePath
      }
  | Dump
      { path :: w ::: FilePath
      }
  deriving (Generic)

instance ParseRecord (SubStep Wrapped) where
  parseRecord = parseRecordWithModifiers modifiers

deriving instance Show (SubStep Unwrapped)

modifiers :: Modifiers
modifiers = defaultModifiers {shortNameModifier = firstLetter}

type Invocation = SubStep Unwrapped

-- | runs the CLI frontend
runCli :: IO Invocation
runCli = unwrapRecord subStepDescription

-- | dispatches the action chosen by the user
dispatch :: Invocation -> IO ()
dispatch (Step p f v c) = stepF p f v c
dispatch (Print p f) = printF p f
dispatch (List p) = listF p
dispatch (Dump p) = dumpF p

-- | dumps the different representations of the GHC pipeline from the users Haskell input file into textfiles
--  (see folder /dump)
dumpF :: [Char] -> IO ()
dumpF fp = do
  fileExists <- (doesFileExist fp)
  if (not fileExists)
    then putStrLn $ "File \"" ++ fp ++ "\" not found"
    else do
      setCurrentDirectory (takeDirectory fp)
      let filePath = takeFileName fp
      cr <- compileToCore filePath
      writeDump cr

-- | lists all the bindings from the Haskell input file provided by the user
listF :: [Char] -> IO ()
listF fp = do
  fileExists <- (doesFileExist fp)
  if (not fileExists)
    then putStrLn $ "File \"" ++ fp ++ "\" not found"
    else do
      setCurrentDirectory (takeDirectory fp)
      let filePath = takeFileName fp
      cr <- compileToCore filePath
      listTopLevelFunctions $ getCoreProgram cr

-- | prints the Core representation the Haskell input file provided by the user
printF :: [Char] -> Maybe [Char] -> IO ()
printF fp fn = do
  fileExists <- (doesFileExist fp)
  if (not fileExists)
    then putStrLn $ "File \"" ++ fp ++ "\" not found"
    else do
      setCurrentDirectory (takeDirectory fp)
      let filePath = takeFileName fp
      cr <- compileToCore filePath
      printCore $ getCoreProgram cr

-- | prints the step by step reduction until head normal form and normal form
--  for every binding provided by the user in the Haskell input file
stepF :: [Char] -> Maybe [Char] -> Maybe Integer -> Maybe Bool -> IO ()
stepF fp fn v c = do
  fileExists <- (doesFileExist fp)
  if (not fileExists)
    then putStrLn $ "File \"" ++ fp ++ "\" not found"
    else do
      setCurrentDirectory (takeDirectory fp)
      let filePath = takeFileName fp
          preludePath = "SteppablePrelude.hs"
      cr <- compileToCore filePath
      spr <- compileToCore preludePath
      let shouldShowComments = fromMaybe False c
      printCoreStepByStepReductionForEveryBinding fn v shouldShowComments (getCoreProgram cr) (getCoreProgram spr)
