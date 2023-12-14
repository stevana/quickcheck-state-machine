{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Test.StateMachine.Lockstep.Simple (
    -- * Test type-level parameters
    MockState
  , Cmd
  , Resp
  , RealHandle
  , MockHandle
  , Test
  , Tag
    -- * Test term-level parameters
  , StateMachineTest(..)
  , Event(..)
    -- * Handle instantiation
  , At(..)
  , (:@)
    -- * Model state
  , Model(..)
    -- * Running the tests
  , prop_sequential
  , prop_parallel
    -- * Translate to n-ary model model
  , fromSimple
    -- * For orphan ToExpr instances
  , Simple
  , NAry.MockHandleN(SimpleToMock)
  ) where

import           Data.Bifunctor
import           Data.Functor.Classes
import           Data.Kind
                   (Type)
import           Data.SOP
import           Data.Typeable
import           Prelude
import           Test.QuickCheck
import           Test.StateMachine
import           Test.StateMachine.Lockstep.Auxiliary
import           Test.StateMachine.Lockstep.NAry
                   (MockState, Tag)

import qualified Test.StateMachine.Lockstep.NAry      as NAry

{-------------------------------------------------------------------------------
  Top-level parameters
-------------------------------------------------------------------------------}

-- | The type of the real handle in the system under test
--
-- The key difference between the " simple " lockstep infrastructure and the
-- n-ary lockstep infrastructure is that the former only supports a single
-- real handle, whereas the latter supports an arbitrary list of them.
data family RealHandle t :: Type

-- | The type of the mock handle
--
-- NOTE: In the n-ary infrastructure, 'MockHandle' is a type family of /two/
-- arguments, because we have a mock handle for each real handle. Here, however,
-- we only /have/ a single real handle, so the " corresponding " real handle
-- is implicitly @RealHandle t@.
data family MockHandle t :: Type

-- | Commands
--
-- In @Cmd t h@, @h@ is the type of the handle
--
-- > Cmd t (RealHandle t)  -- for the system under test
-- > Cmd t (MockHandle t)  -- for the mock
data family Cmd t :: Type -> Type

-- | Responses
--
-- In @Resp t h@, @h@ is the type of the handle
--
-- > Resp t (RealHandle t)  -- for the system under test
-- > Resp t (MockHandle t)  -- for the mock
data family Resp t :: Type -> Type

{-------------------------------------------------------------------------------
  Default handle instantiation
-------------------------------------------------------------------------------}

type family Test (f :: Type -> Type) :: Type where
  Test (Cmd  t) = t
  Test (Resp t) = t

-- @f@ will be instantiated with @Cmd@ or @Resp@
-- @r@ will be instantiated with 'Symbolic' or 'Concrete'
newtype At f r = At { unAt :: f (Reference (RealHandle (Test f)) r) }
type    f :@ r = At f r

{-------------------------------------------------------------------------------
  Simplified model
-------------------------------------------------------------------------------}

data Model t r = Model {
      modelState :: MockState t
    , modelRefs  :: [(Reference (RealHandle t) r, MockHandle t)]
    }

modelToSimple :: NAry.Model (Simple t) r -> Model t r
modelToSimple NAry.Model{modelRefss = NAry.Refss (NAry.Refs rs :* Nil), ..} = Model {
      modelState = modelState
    , modelRefs  = map (second unSimpleToMock) rs
    }

{-------------------------------------------------------------------------------
  Simplified event
-------------------------------------------------------------------------------}

data Event t r = Event {
      before   :: Model t    r
    , cmd      :: Cmd   t :@ r
    , after    :: Model t    r
    , mockResp :: Resp t (MockHandle t)
    }

eventToSimple :: (Functor (Cmd t), Functor (Resp t))
              => NAry.Event (Simple t) r -> Event t r
eventToSimple NAry.Event{..} = Event{
      before   = modelToSimple before
    , cmd      = cmdAtToSimple cmd
    , after    = modelToSimple after
    , mockResp = respMockToSimple mockResp
    }

{-------------------------------------------------------------------------------
  Wrap and unwrap
-------------------------------------------------------------------------------}

cmdAtFromSimple :: Functor (Cmd t) => Cmd t :@ r -> NAry.Cmd (Simple t) NAry.:@ r
cmdAtFromSimple = NAry.At . SimpleCmd . fmap NAry.FlipRef . unAt

cmdAtToSimple :: Functor (Cmd t) => NAry.Cmd (Simple t) NAry.:@ r -> Cmd t :@ r
cmdAtToSimple = At . fmap NAry.unFlipRef . unSimpleCmd . NAry.unAt

cmdMockToSimple :: Functor (Cmd t)
                => NAry.Cmd (Simple t) (NAry.MockHandleN (Simple t)) '[RealHandle t]
                -> Cmd t (MockHandle t)
cmdMockToSimple = fmap unSimpleToMock . unSimpleCmd

cmdRealToSimple :: Functor (Cmd t)
                => NAry.Cmd (Simple t) I '[RealHandle t]
                -> Cmd t (RealHandle t)
cmdRealToSimple = fmap unI . unSimpleCmd

respMockFromSimple :: Functor (Resp t)
                   => Resp t (MockHandle t)
                   -> NAry.Resp (Simple t) (NAry.MockHandleN (Simple t)) '[RealHandle t]
respMockFromSimple = SimpleResp . fmap SimpleToMock

respMockToSimple :: Functor (Resp t)
                 => NAry.Resp (Simple t) (NAry.MockHandleN (Simple t)) '[RealHandle t]
                 -> Resp t (MockHandle t)
respMockToSimple = fmap unSimpleToMock . unSimpleResp

respRealFromSimple :: Functor (Resp t)
                   => Resp t (RealHandle t)
                   -> NAry.Resp (Simple t) I '[RealHandle t]
respRealFromSimple = SimpleResp . fmap I

{-------------------------------------------------------------------------------
  User defined values
-------------------------------------------------------------------------------}

-- | State machine test
--
-- This captures the design patterns sketched in
-- <https://well-typed.com/blog/2019/01/qsm-in-depth/> for the case where there
-- is exactly one real handle. See "Test.StateMachine.Lockstep.NAry" for the
-- generalization to @n@ handles.
data StateMachineTest t =
    ( Typeable t
      -- Response
    , Eq   (Resp t (MockHandle t))
    , Show (Resp t (Reference (RealHandle t) Symbolic))
    , Show (Resp t (Reference (RealHandle t) Concrete))
    , Show (Resp t (MockHandle t))
    , Traversable (Resp t)
      -- Command
    , Show (Cmd t (Reference (RealHandle t) Symbolic))
    , Show (Cmd t (Reference (RealHandle t) Concrete))
    , Traversable (Cmd t)
      -- Real handles
    , Eq      (RealHandle t)
    , Show    (RealHandle t)
    , CanDiff (RealHandle t)
      -- Mock handles
    , Eq      (MockHandle t)
    , Show    (MockHandle t)
    , CanDiff (MockHandle t)
    , CanDiff (NAry.MockHandleN (Simple t) (RealHandle t))
      -- Mock state
    , Show    (MockState t)
    , CanDiff (MockState t)
      -- Tags
    , Show (Tag t)
      -- Model
    , CanDiff (NAry.Model (Simple t) Concrete)
    ) => StateMachineTest {
      runMock    :: Cmd t (MockHandle t) -> MockState t -> (Resp t (MockHandle t), MockState t)
    , runReal    :: Cmd t (RealHandle t) -> IO (Resp t (RealHandle t))
    , initMock   :: MockState t
    , newHandles :: forall h. Resp t h -> [h]
    , generator  :: Model t Symbolic -> Maybe (Gen (Cmd t :@ Symbolic))
    , shrinker   :: Model t Symbolic -> Cmd t :@ Symbolic -> [Cmd t :@ Symbolic]
    , cleanup    :: Model t Concrete -> IO ()
    , tag        :: [Event t Symbolic] -> [Tag t]
    }

data Simple t

type instance NAry.MockState   (Simple t) = MockState t
type instance NAry.RealHandles (Simple t) = '[RealHandle t]
type instance NAry.Tag         (Simple t) = Tag t

data instance NAry.Cmd (Simple _) _f _hs where
    SimpleCmd :: Cmd t (f h) -> NAry.Cmd (Simple t) f '[h]

data instance NAry.Resp (Simple _) _f _hs where
    SimpleResp :: Resp t (f h) -> NAry.Resp (Simple t) f '[h]

newtype instance NAry.MockHandleN (Simple t) (RealHandle t) =
    SimpleToMock { unSimpleToMock :: MockHandle t }

unSimpleCmd :: NAry.Cmd (Simple t) f '[h] -> Cmd t (f h)
unSimpleCmd (SimpleCmd cmd) = cmd

unSimpleResp :: NAry.Resp (Simple t) f '[h] -> Resp t (f h)
unSimpleResp (SimpleResp resp) = resp

instance ( Functor (Resp t)
         , Eq (Resp t (MockHandle t))
         , Eq (MockHandle t)
         ) => Eq (NAry.Resp (Simple t) (NAry.MockHandleN (Simple t)) '[RealHandle t]) where
  SimpleResp r == SimpleResp r' = (unSimpleToMock <$> r) == (unSimpleToMock <$> r')

instance ( Functor (Resp t)
         , Show (Resp t (MockHandle t))
         ) => Show (NAry.Resp (Simple t) (NAry.MockHandleN (Simple t)) '[RealHandle t]) where
  show (SimpleResp r) = show (unSimpleToMock <$> r)

instance ( Functor (Resp t)
         , Show (Resp t (Reference (RealHandle t) r))
         , Show1 r
         ) => Show (NAry.Resp (Simple t) (NAry.FlipRef r) '[RealHandle t]) where
  show (SimpleResp r) = show (NAry.unFlipRef <$> r)

instance ( Functor (Cmd t)
         , Show (Cmd t (Reference (RealHandle t) r))
         , Show1 r
         ) => Show (NAry.Cmd (Simple t) (NAry.FlipRef r) '[RealHandle t]) where
  show (SimpleCmd r) = show (NAry.unFlipRef <$> r)

deriving stock instance Eq   (MockHandle t) => Eq   (NAry.MockHandleN (Simple t) (RealHandle t))
deriving stock instance Show (MockHandle t) => Show (NAry.MockHandleN (Simple t) (RealHandle t))

instance Traversable (Resp t) => NTraversable (NAry.Resp (Simple t)) where
  nctraverse _ f (SimpleResp x) = SimpleResp <$> traverse (f ElemHead) x

instance Traversable (Cmd t) => NTraversable (NAry.Cmd (Simple t)) where
  nctraverse _ f (SimpleCmd x) = SimpleCmd <$> traverse (f ElemHead) x

fromSimple :: StateMachineTest t -> NAry.StateMachineTest (Simple t) IO
fromSimple StateMachineTest{..} = NAry.StateMachineTest {
      runMock    = \cmd st -> first respMockFromSimple (runMock (cmdMockToSimple cmd) st)
    , runReal    = \cmd -> respRealFromSimple <$> runReal (cmdRealToSimple cmd)
    , initMock   = initMock
    , newHandles = \r -> Comp (newHandles (unSimpleResp r)) :* Nil
    , generator  = \m     -> fmap cmdAtFromSimple <$> generator (modelToSimple m)
    , shrinker   = \m cmd ->      cmdAtFromSimple <$> shrinker  (modelToSimple m) (cmdAtToSimple cmd)
    , cleanup    = cleanup   . modelToSimple
    , tag        = tag . map eventToSimple
    }

{-------------------------------------------------------------------------------
  Running the tests
-------------------------------------------------------------------------------}

prop_sequential :: StateMachineTest t
                -> Maybe Int   -- ^ (Optional) minimum number of commands
                -> Property
prop_sequential = NAry.prop_sequential . fromSimple

prop_parallel :: StateMachineTest t
              -> Maybe Int   -- ^ (Optional) minimum number of commands
              -> Property
prop_parallel = NAry.prop_parallel . fromSimple
