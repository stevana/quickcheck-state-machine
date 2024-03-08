{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

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

module EchoWithTracer
  ( prop_echoWithTracerSequentialOK
  , prop_echoWithTracerParallelFail
  , prop_echoWithTracerNParallelFail
  , prop_echoWithTracerParallelOK
  , prop_echoWithTracerNParallelOK
  )
  where


import           Data.Bifunctor
import           Data.Kind                     (Type)
import           GHC.Generics                  (Generic, Generic1)
import           GHC.List                      (List)
import           Prelude
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.StateMachine
import           Test.StateMachine.TreeDiff
import           Test.StateMachine.Types       as QC
import qualified Test.StateMachine.Types.Rank2 as Rank2
import       Test.StateMachine.Utils
import           UnliftIO

------------------------------------------------------------------------
-- | Spec for echo.

-- | This one will work as there is only one machine running
prop_echoWithTracerSequentialOK :: Property
prop_echoWithTracerSequentialOK = forAllCommands smUnused Nothing $ \cmds -> monadicIO $ do
    (tracer, getTraces) <- run $ recordingTracerIORef
    (hist, _, res, _prop) <- runCommandsWithSetup (echoWithTracerSM tracer) cmds
    let l = howManyInsS cmds
    traces <- run getTraces
    let countInTraces = countTraceIns traces
    liftProperty $ counterexample ("More 'In's in trace (" <> show countInTraces <>") than in commands(" <> show l <>")") $ l == countInTraces
    prettyCommands smUnused hist (res === Ok)

-- | Defining the tracer outside of the machine:
-- NOTE This will return wrong results!
prop_echoWithTracerParallelFail :: Property
prop_echoWithTracerParallelFail = forAllParallelCommands smUnused Nothing $ \cmds ->
    monadicIO $ do
      (tracer, getTraces) <- run $ recordingTracerIORef
      h <- runParallelCommandsWithSetup (echoWithTracerSM tracer) cmds
      let l = howManyInsP cmds
      traces <- run getTraces
      let countInTraces = countTraceIns traces
      liftProperty $ counterexample ("More 'In's in trace (" <> show countInTraces <>") than in commands(" <> show l <>")") $ l == countInTraces
      prettyParallelCommands cmds h

-- | Defining the tracer inside the machine is OK
prop_echoWithTracerParallelOK :: Property
prop_echoWithTracerParallelOK = forAllParallelCommands smUnused Nothing $ \cmds ->
    monadicIO $ do
      h <- runParallelCommandsWithSetup echoWithTracerSM' cmds
      prettyParallelCommands cmds h


-- | Same as prop_echoWithTracerParallelFail
prop_echoWithTracerNParallelFail :: Int -> Property
prop_echoWithTracerNParallelFail np = forAllNParallelCommands smUnused np $ \cmds -> monadicIO $ do
    (tracer, getTraces) <- run $ recordingTracerIORef
    h <- runNParallelCommandsWithSetup (echoWithTracerSM tracer) cmds
    let l = howManyInsNP cmds
    traces <- run getTraces
    let countInTraces = countTraceIns traces
    liftProperty $ counterexample ("More 'In's in trace (" <> show countInTraces <>") than in commands(" <> show l <>")") $ l == countInTraces
    prettyNParallelCommands cmds h

-- | Same as prop_echoWithTracerParallelOK
prop_echoWithTracerNParallelOK :: Int -> Property
prop_echoWithTracerNParallelOK np = forAllNParallelCommands smUnused np $ \cmds -> monadicIO $ do
    h <- runNParallelCommandsWithSetup echoWithTracerSM' cmds
    prettyNParallelCommands cmds h

-- | Echo API.

data Env = Env
    { _buf :: TVar (Maybe String)
    , count :: TVar Int -- Nmber of 'In's received
    }

-- | Create a new environment.
mkEnv :: IO Env
mkEnv = do
    newBuf <- newTVarIO Nothing
    newCount <- newTVarIO  0
    pure $ Env newBuf newCount

-- | Input a string. Returns 'True' iff the buffer was empty and the given
-- string was added to it. Increases the number of 'In" commands encountered.
input :: Env -> String -> IO Bool
input (Env mBuf ct) str = atomically $ do
    resBuf <- readTVar mBuf
    modifyTVar ct (+1)
    case resBuf of
        Nothing -> writeTVar mBuf (Just str) >> return True
        Just _  -> return False

-- | Output the buffer contents.
o :: Env -> IO (Maybe String)
o (Env mBuf _) = atomically $ do
    res <- readTVar mBuf
    writeTVar mBuf Nothing
    return res

-- | Create a 'Tracer' that stores all events in an 'IORef' that is atomically
-- updated. The second return value lets you obtain the events recorded so far
-- (from oldest to newest). Obtaining the events does not erase them.
recordingTracerIORef :: IO (ev -> IO (), IO [ev])
recordingTracerIORef = newIORef [] >>= \ref -> return
    ( \ev -> atomicModifyIORef' ref $ \evs -> (ev:evs, ())
    , reverse <$> readIORef ref
    )

------------------------------------------------------------------------

smUnused :: StateMachine Model Action IO Response
smUnused = StateMachine
    { initModel = Empty -- At the beginning of time nothing was received.
    , transition = transitions
    , precondition = preconditions
    , postcondition = postconditions
    , QC.generator = EchoWithTracer.generator
    , invariant = Nothing
    , QC.shrinker = EchoWithTracer.shrinker
    , QC.semantics = e
    , QC.mock = EchoWithTracer.mock
    , cleanup = noCleanup
    , finalCheck = pure Nothing
    }
  where
    e = error "SUT must not be used"

echoWithTracerSM :: (TraceEvent -> IO ()) -> IO (StateMachine Model Action IO Response)
echoWithTracerSM tr = do
  env <- mkEnv
  pure $ StateMachine
    { initModel = Empty -- At the beginning of time nothing was received.
    , transition = transitions
    , precondition = preconditions
    , postcondition = postconditions
    , QC.generator = EchoWithTracer.generator
    , invariant = Nothing
    , QC.shrinker = EchoWithTracer.shrinker
    , QC.semantics = EchoWithTracer.semantics env tr
    , QC.mock = EchoWithTracer.mock
    , cleanup = noCleanup
    , finalCheck = pure $ Just $ property False -- Nothing
    }

echoWithTracerSM' :: IO (StateMachine Model Action IO Response)
echoWithTracerSM' = do
  env <- mkEnv
  (tracer, getTraces) <- recordingTracerIORef
  pure $ StateMachine
    { initModel = Empty -- At the beginning of time nothing was received.
    , transition = transitions
    , precondition = preconditions
    , postcondition = postconditions
    , QC.generator = EchoWithTracer.generator
    , invariant = Nothing
    , QC.shrinker = EchoWithTracer.shrinker
    , QC.semantics = EchoWithTracer.semantics env tracer
    , QC.mock = EchoWithTracer.mock
    , cleanup = noCleanup
    , finalCheck = do
        finalCount <- readTVarIO $ count env
        traces <- getTraces
        let countInTraces = countTraceIns traces
        pure $ Just $ counterexample ("More 'In's in traces ("<> show countInTraces <> ") than in model (" <> show finalCount <> ")")  $
            finalCount == countInTraces -- || finalCount == (countInTraces - 1)
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
    [
        --In <$> arbitrary
        In <$> (oneof $ pure <$> ["a", "b", "c", "d", "e"])
    , return Echo
    ]

-- | Trivial shrinker.
shrinker :: Model Symbolic -> Action Symbolic -> [Action Symbolic]
shrinker _ _ = []

-- | Here we'd do the dispatch to the actual SUT.
semantics :: Env -> (TraceEvent -> IO ()) -> Action Concrete -> IO (Response Concrete)
semantics env tr (In str) = do
    success <- input env str
    tr $ TraceReceiveIn str
    return $ if success
               then InAck
               else ErrFull
semantics env tr Echo = do
    tr TraceReceiveEcho
    maybe ErrEmpty Out <$> o env

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

data TraceEvent = TraceReceiveIn String
    | TraceReceiveEcho
    deriving Show
countTraceIns :: [TraceEvent] -> Int
countTraceIns = length . filter isIn
    where
        isIn (TraceReceiveIn _) = True
        isIn _ = False

-- | See how many `In` commands were issued
howManyInsS :: Commands Action resp -> Int
howManyInsS cmds =
  length
  $ filter (\case
               In{} -> True
               _ -> False)
  $ map getCommand
  $ unCommands cmds

-- | See how many `In` commands were issued
howManyInsP :: ParallelCommandsF Pair Action resp -> Int
howManyInsP cmds =
  length
  $ filter (\case
               In{} -> True
               _ -> False)
  $ map getCommand
  $ unCommands (prefix cmds)
  ++ concatMap (uncurry (++) . bimap unCommands unCommands . fromPair) (suffixes cmds)

-- | See how many `In` commands were issued
howManyInsNP :: forall resp. ParallelCommandsF List Action resp -> Int
howManyInsNP cmds =
  length
  $ filter (\case
               In{} -> True
               _ -> False)
  $ map getCommand
  $ unCommands (prefix cmds)
  ++ concat (concat (map (map unCommands) $ suffixes cmds))