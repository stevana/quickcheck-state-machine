-----------------------------------------------------------------------------
-- |
-- Module      :  Test.StateMachine
-- Copyright   :  (C) 2017, ATS Advanced Telematic Systems GmbH
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Stevan Andjelkovic <stevan.andjelkovic@strath.ac.uk>
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- The main module for state machine based testing, it contains
-- combinators that help you build sequential and parallel properties.
--
-----------------------------------------------------------------------------

module Test.StateMachine

  ( -- * Sequential property combinators
    forAllCommands
  , existsCommands
  , runCommands
  , prettyCommands
  , prettyCommands'
  , checkCommandNames
  , coverCommandNames
  , commandNames
  , commandNamesInOrder
  , saveCommands
  , runSavedCommands
  , showLabelledExamples
  , showLabelledExamples'
  , noCleanup

  -- * Parallel property combinators
  , forAllParallelCommands
  , forAllNParallelCommands
  , forAllParallelCommandsNTimes
  , forAllNParallelCommandsNTimes
  , runNParallelCommands
  , runParallelCommands
  , runParallelCommands'
  , runNParallelCommands'
  , prettyNParallelCommands
  , prettyParallelCommands
  , prettyParallelCommandsWithOpts
  , prettyNParallelCommandsWithOpts
  , checkCommandNamesParallel
  , coverCommandNamesParallel
  , commandNamesParallel

    -- * Types
  , StateMachine(StateMachine)
  , Concrete
  , Symbolic
  , Reference
  , concrete
  , reference
  , Opaque(..)
  , opaque
  , Reason(..)
  , GenSym
  , genSym
  , CommandNames(..)

  , module Test.StateMachine.Logic

  -- * Diffing class
  , CanDiff (..)
  ) where

import           Prelude
                   ()

import           Test.StateMachine.ConstructorName
import           Test.StateMachine.Diffing
import           Test.StateMachine.Logic
import           Test.StateMachine.Parallel
import           Test.StateMachine.Sequential
import           Test.StateMachine.Types
