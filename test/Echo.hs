{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Echo
-- Copyright   :  (C) 2018, Damian Nadales
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Stevan Andjelkovic <stevan.andjelkovic@strath.ac.uk>
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
------------------------------------------------------------------------

module Echo
  ( mkEnv
  , prop_echoOK
  , prop_echoNParallelOK
  , prop_echoParallelOK
  )
  where

import           Data.Kind
                   (Type)
import           GHC.Generics
                   (Generic, Generic1)
import           Prelude
import           Test.QuickCheck
                   (Gen, Property, arbitrary, oneof, (===))
import           Test.QuickCheck.Monadic
                   (monadicIO)
import           UnliftIO
                   (TVar, atomically, liftIO, newTVarIO, readTVar,
                   writeTVar)

import           Test.StateMachine
import           Test.StateMachine.Types as QC
import qualified Test.StateMachine.Types.Rank2 as Rank2

------------------------------------------------------------------------

-- | Echo API.

data Env = Env
    { _buf :: TVar (Maybe String) }

-- | Create a new environment.
mkEnv :: IO Env
mkEnv = Env <$> newTVarIO Nothing

-- | Input a string. Returns 'True' iff the buffer was empty and the given
-- string was added to it.
input :: Env -> String -> IO Bool
input (Env mBuf) str = atomically $ do
    res <- readTVar mBuf
    case res of
        Nothing -> writeTVar mBuf (Just str) >> return True
        Just _  -> return False

-- | Output the buffer contents.
output :: Env -> IO (Maybe String)
output (Env mBuf) = atomically $ do
    res <- readTVar mBuf
    writeTVar mBuf Nothing
    return res

------------------------------------------------------------------------

-- | Spec for echo.

prop_echoOK :: Property
prop_echoOK = forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
    (hist, _, res) <- runCommandsWithSetup echoSM cmds
    prettyCommands smUnused hist (res === Ok)

prop_echoParallelOK :: Property
prop_echoParallelOK = forAllParallelCommands smUnused Nothing $ \cmds -> monadicIO $ do
    prettyParallelCommands cmds =<< runParallelCommandsWithSetup echoSM cmds

prop_echoNParallelOK :: Int -> Property
prop_echoNParallelOK np = forAllNParallelCommands smUnused np $ \cmds -> monadicIO $ do
    prettyNParallelCommands cmds =<< runNParallelCommandsWithSetup echoSM cmds

smUnused :: StateMachine Model Action IO Response
smUnused = StateMachine
    { initModel = Empty -- At the beginning of time nothing was received.
    , transition = transitions
    , precondition = preconditions
    , postcondition = postconditions
    , QC.generator = Echo.generator
    , invariant = Nothing
    , QC.shrinker = Echo.shrinker
    , QC.semantics = e
    , QC.mock = Echo.mock
    , cleanup = noCleanup
    }
  where
    e = error "SUT must not be used"

echoSM :: IO (StateMachine Model Action IO Response)
echoSM  = do
  env <- mkEnv
  pure $ StateMachine
    { initModel = Empty -- At the beginning of time nothing was received.
    , transition = transitions
    , precondition = preconditions
    , postcondition = postconditions
    , QC.generator = Echo.generator
    , invariant = Nothing
    , QC.shrinker = Echo.shrinker
    , QC.semantics = Echo.semantics env
    , QC.mock = Echo.mock
    , cleanup = noCleanup
    }

transitions :: Model r -> Action r -> Response r -> Model r
transitions Empty   (In str) _   = Buf str
transitions (Buf _) Echo     _   = Empty
transitions Empty   Echo     _   = Empty
-- TODO: qcsm will match the case below. However we don't expect this to happen!
transitions (Buf str) (In _)   _ = Buf str -- Dummy response
    -- error "This shouldn't happen: input transition with full buffer"

-- | There are no preconditions for this model.
preconditions :: Model Symbolic -> Action Symbolic -> Logic
preconditions _ _ = Top

-- | Post conditions for the system.
postconditions :: Model Concrete -> Action Concrete -> Response Concrete -> Logic
postconditions Empty     (In _) InAck     = Top
postconditions (Buf _)   (In _) ErrFull   = Top
postconditions _         (In _) _         = Bot
postconditions Empty     Echo   ErrEmpty  = Top
postconditions Empty     Echo   _         = Bot
postconditions (Buf str) Echo   (Out out) = str .== out
postconditions (Buf _)   Echo   _         = Bot

-- | Generator for symbolic actions.
generator :: Model Symbolic -> Maybe (Gen (Action Symbolic))
generator _ =  Just $ oneof
    [ In <$> arbitrary
    , return Echo
    ]

-- | Trivial shrinker.
shrinker :: Model Symbolic -> Action Symbolic -> [Action Symbolic]
shrinker _ _ = []

-- | Here we'd do the dispatch to the actual SUT.
semantics :: Env -> Action Concrete -> IO (Response Concrete)
semantics env (In str) = do
    success <- input env str
    return $ if success
             then InAck
             else ErrFull
semantics env Echo = maybe ErrEmpty Out <$> output env

-- | What is the mock for?
mock :: Model Symbolic -> Action Symbolic -> GenSym (Response Symbolic)
mock Empty (In _)   = return InAck
mock (Buf _) (In _) = return ErrFull
mock Empty Echo     = return ErrEmpty
mock (Buf str) Echo = return (Out str)

deriving anyclass instance ToExpr (Model Concrete)

-- | The model contains the last string that was communicated in an input
-- action.
data Model (r :: Type -> Type)
    = -- | The model hasn't been initialized.
      Empty
    | -- | Last input string (a buffer with size one).
      Buf String
  deriving stock (Eq, Show, Generic)

-- | Actions supported by the system.
data Action (r :: Type -> Type)
    = -- | Input a string, which should be echoed later.
      In String
      -- | Request a string output.
    | Echo
  deriving stock (Show, Generic1)
  deriving anyclass (Rank2.Foldable, Rank2.Traversable, Rank2.Functor, CommandNames)

-- | The system gives a single type of output response, containing a string
-- with the input previously received.
data Response (r :: Type -> Type)
    = -- | Input acknowledgment.
      InAck
      -- | The previous action wasn't an input, so there is no input to echo.
      -- This is: the buffer is empty.
    | ErrEmpty
      -- | There is already a string in the buffer.
    | ErrFull
      -- | Output string.
    | Out String
  deriving stock (Show, Generic1)
  deriving anyclass (Rank2.Foldable, Rank2.Traversable, Rank2.Functor)
