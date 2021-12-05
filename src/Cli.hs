{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Cli (runCli) where

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
import Prelude hiding (FilePath)
import qualified Prelude as P (FilePath)

type FilePath = P.FilePath <?> "Haskell source file to step through"

type FunctionName = String <?> "Top level function to step through"

type VerbosityLevel = Maybe Integer <?> "Verbosity level between 1 and 3"

subStepDescription :: Text
subStepDescription = "The Haskell Substitution Stepper"

data SubStep w
  = Step
      { path :: w ::: FilePath,
        function :: w ::: FunctionName,
        verbose :: w ::: VerbosityLevel
      }
  | Print
      { path :: w ::: FilePath,
        function :: w ::: FunctionName
      }
  | List
      { path :: w ::: FilePath,
        function :: w ::: FunctionName
      }
  deriving (Generic)

instance ParseRecord (SubStep Wrapped) where
  parseRecord = parseRecordWithModifiers modifiers

deriving instance Show (SubStep Unwrapped)

modifiers :: Modifiers
modifiers = defaultModifiers {shortNameModifier = firstLetter}

runCli :: IO (SubStep Unwrapped)
runCli = unwrapRecord subStepDescription