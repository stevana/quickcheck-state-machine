{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Test.StateMachine.Lockstep.NAry (
    -- * Test type-level parameters
    MockState
  , Cmd
  , Resp
  , RealHandles
  , MockHandleN
  , Test
  , Tag
    -- * Test term-level parameters
  , StateMachineTest(..)
  , Event(..)
  , hoistStateMachineTest
    -- * Handle instantiation
  , At(..)
  , (:@)
    -- * Model state
  , Model(..)
  , Refs(..)
  , Refss(..)
  , FlipRef(..)
    -- * Running the tests
  , prop_sequential
  , prop_parallel
    -- * Examples
  , showLabelledExamples'
  , showLabelledExamples
  -- * Translate to state machine model
  , toStateMachine
  ) where

import           Data.Functor.Classes
import           Data.Kind
                   (Type)
import           Data.Maybe
                   (fromJust)
import           Data.Semigroup                       hiding
                   (All)
import           Data.SOP
import           Data.Typeable
import           GHC.Generics
                   (Generic)
import           Prelude
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.StateMachine                    hiding
                   (showLabelledExamples, showLabelledExamples')

import qualified Data.Monoid                          as M
import qualified Test.StateMachine.Labelling          as Label
import qualified Test.StateMachine.Sequential         as Seq
import qualified Test.StateMachine.Types              as QSM
import qualified Test.StateMachine.Types.Rank2        as Rank2

import           Test.StateMachine.Lockstep.Auxiliary

{-------------------------------------------------------------------------------
  Test type-level parameters
-------------------------------------------------------------------------------}

-- | Mock state
--
-- The @t@ argument (here and elsewhere) is a type-level tag that combines all
-- aspects of the test; it does not need any term-level constructors
--
-- > data MyTest
-- > type instance MockState MyTest = ..
type family MockState t :: Type

-- | Type-level list of the types of the handles in the system under test
--
-- NOTE: If your system under test only requires a single real handle, you
-- might consider using "Test.StateMachine.Lockstep.Simple" instead.
type family RealHandles t :: [Type]

-- | Mock handles
--
-- For each real handle @a@, @MockHandleN t a@ is the corresponding mock handle.
data family MockHandleN t a :: Type

-- | Commands
--
-- In @Cmd t f hs@, @hs@ is the list of real handle types, and @f@ is some
-- functor applied to each of them. Two typical instantiations are
--
-- > Cmd t I              (RealHandles t)   -- for the system under test
-- > Cmd t (MockHandleN t) (RealHandles t)   -- for the mock
data family Cmd t :: (Type -> Type) -> [Type] -> Type

-- | Responses
--
-- The type arguments are similar to those of @Cmd@. Two typical instances:
--
-- > Resp t I              (RealHandles t)  -- for the system under test
-- > Resp t (MockHandleN t) (RealHandles t)  -- for the mock
data family Resp t :: (Type -> Type) -> [Type] -> Type

-- | Tags
--
-- Tags are used when labelling execution runs in 'prop_sequential', as well as
-- when looking for minimal examples with a given label
-- ('showLabelledExamples').
type family Tag t :: Type

{-------------------------------------------------------------------------------
  Reference environments
-------------------------------------------------------------------------------}

-- | Relation between real and mock references for single handle type @a@
newtype Refs t r a = Refs { unRefs :: [(Reference a r, MockHandleN t a)] }
  deriving newtype (Semigroup, Monoid, Generic)

deriving
  stock
  instance (Show1 r, Show a, Show (MockHandleN t a)) => Show (Refs t r a)

-- | Relation between real and mock references for /all/ handle types
newtype Refss t r = Refss { unRefss :: NP (Refs t r) (RealHandles t) }

instance ( Show1 r
         , All (And Show (Compose Show (MockHandleN t))) (RealHandles t)
         ) => Show (Refss t r) where
  show = unlines
       . hcollapse
       . hcmap (Proxy @(And Show (Compose Show (MockHandleN t)))) showOne
       . unRefss
    where
      showOne :: (Show a, Show (MockHandleN t a))
              => Refs t r a -> K String a
      showOne = K . show

instance SListI (RealHandles t) => Semigroup (Refss t r) where
  Refss rss <> Refss rss' = Refss $ hzipWith (<>) rss rss'

instance SListI (RealHandles t) => Monoid (Refss t r) where
  mempty = Refss $ hpure (Refs mempty)

{-------------------------------------------------------------------------------
  Default instantiation to handles
-------------------------------------------------------------------------------}

type family Test (f :: (Type -> Type) -> [Type] -> Type) :: Type where
  Test (Cmd  t) = t
  Test (Resp t) = t

newtype FlipRef r h = FlipRef { unFlipRef :: Reference h r }
  deriving stock (Show)

-- @f@ will be instantiated with @Cmd@ or @Resp@
-- @r@ will be instantiated with 'Symbolic' or 'Concrete'
newtype At f r = At { unAt :: f (FlipRef r) (RealHandles (Test f)) }
type    f :@ r = At f r

deriving
  stock
  instance (Show (f (FlipRef r) (RealHandles (Test f)))) => Show (At f r)

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

data Model t r = Model {
      modelState :: MockState t
    , modelRefss :: Refss t r
    }
  deriving stock (Generic)

deriving stock instance ( Show1 r
                        , Show (MockState t)
                        , All (And Show (Compose Show (MockHandleN t))) (RealHandles t)
                        ) => Show (Model t r)

initModel :: StateMachineTest t m -> Model t r
initModel StateMachineTest{..} = Model initMock (Refss (hpure (Refs [])))

{-------------------------------------------------------------------------------
  High level API
-------------------------------------------------------------------------------}

-- | State machine test
--
-- This captures the design patterns sketched in
-- <https://well-typed.com/blog/2019/01/qsm-in-depth/>.
data StateMachineTest t m =
    ( Monad m
    -- Requirements on the handles
    , All Typeable                                       (RealHandles t)
    , All Eq                                             (RealHandles t)
    , All (And Show    (Compose Show    (MockHandleN t))) (RealHandles t)
    , All (And CanDiff (Compose CanDiff (MockHandleN t))) (RealHandles t)
    -- Response
    , NTraversable (Resp t)
    , Eq   (Resp t (MockHandleN t)     (RealHandles t))
    , Show (Resp t (MockHandleN t)     (RealHandles t))
    , Show (Resp t (FlipRef Symbolic) (RealHandles t))
    , Show (Resp t (FlipRef Concrete) (RealHandles t))
    -- Command
    , NTraversable (Cmd t)
    , Show (Cmd t (FlipRef Symbolic) (RealHandles t))
    , Show (Cmd t (FlipRef Concrete) (RealHandles t))
    -- MockState
    , Show   (MockState t)
    , CanDiff (MockState t)
    -- Tags
    , Show (Tag t)
    -- Model
    , CanDiff (Model t Concrete)
    ) => StateMachineTest {
      runMock    :: Cmd t (MockHandleN t) (RealHandles t) -> MockState t -> (Resp t (MockHandleN t) (RealHandles t), MockState t)
    , runReal    :: Cmd t I              (RealHandles t) -> m (Resp t I (RealHandles t))
    , initMock   :: MockState t
    , newHandles :: forall f. Resp t f (RealHandles t) -> NP ([] :.: f) (RealHandles t)
    , generator  :: Model t Symbolic -> Maybe (Gen (Cmd t :@ Symbolic))
    , shrinker   :: Model t Symbolic -> Cmd t :@ Symbolic -> [Cmd t :@ Symbolic]
    , cleanup    :: Model t Concrete -> m ()
    , tag        :: [Event t Symbolic] -> [Tag t]
    }

hoistStateMachineTest :: Monad n
                      => (forall a. m a -> n a)
                      -> StateMachineTest t m
                      -> StateMachineTest t n
hoistStateMachineTest f StateMachineTest {..} = StateMachineTest {
      runMock    = runMock
    , runReal    = f . runReal
    , initMock   = initMock
    , newHandles = newHandles
    , generator  = generator
    , shrinker   = shrinker
    , cleanup    = f . cleanup
    , tag        = tag
    }

semantics :: StateMachineTest t m
          -> Cmd t :@ Concrete
          -> m (Resp t :@ Concrete)
semantics StateMachineTest{..} (At c) =
    At . ncfmap (Proxy @Typeable) (const wrapConcrete) <$>
      runReal (nfmap (const unwrapConcrete) c)

unwrapConcrete :: FlipRef Concrete a -> I a
unwrapConcrete = I . concrete . unFlipRef

wrapConcrete :: Typeable a => I a -> FlipRef Concrete a
wrapConcrete = FlipRef . reference  . unI

-- | Turn @Cmd@ or @Resp@ in terms of (symbolic or concrete) references to
-- real handles into a command in terms of mock handles.
--
-- This is isomorphic to
--
-- > toMock :: Refss t Symbolic
--          -> Cmd (FlipRef r) (Handles t)
--          -> Cmd  ToMock     (Handles t)
toMockHandleNs :: (NTraversable f, t ~ Test f, All Eq (RealHandles t), Eq1 r)
              => Refss t r -> f :@ r -> f (MockHandleN t) (RealHandles t)
toMockHandleNs rss (At fr) =
    ncfmap (Proxy @Eq) (\pf -> find (unRefss rss) pf . unFlipRef) fr
  where
    find :: (Eq a, Eq1 r)
         => NP (Refs t r) (RealHandles t)
         -> Elem (RealHandles t) a
         -> Reference a r -> MockHandleN t a
    find refss ix r = unRefs (npAt refss ix) ! r

step :: Eq1 r
     => StateMachineTest t m
     -> Model t r
     -> Cmd t :@ r
     -> (Resp t (MockHandleN t) (RealHandles t), MockState t)
step StateMachineTest{..} (Model st rss) cmd =
    runMock (toMockHandleNs rss cmd) st

data Event t r = Event {
      before   :: Model t    r
    , cmd      :: Cmd   t :@ r
    , after    :: Model t    r
    , mockResp :: Resp t (MockHandleN t) (RealHandles t)
    }

lockstep :: forall t m r. Eq1 r
         => StateMachineTest t m
         -> Model t    r
         -> Cmd   t :@ r
         -> Resp  t :@ r
         -> Event t    r
lockstep sm@StateMachineTest{..} m@(Model _ rss) c (At resp) = Event {
      before   = m
    , cmd      = c
    , after    = Model st' (rss <> rss')
    , mockResp = resp'
    }
  where
    (resp', st') = step sm m c

    rss' :: Refss t r
    rss' = zipHandles (newHandles resp) (newHandles resp')

transition :: Eq1 r
           => StateMachineTest t m
           -> Model t    r
           -> Cmd   t :@ r
           -> Resp  t :@ r
           -> Model t    r
transition sm m c = after . lockstep sm m c

postcondition :: StateMachineTest t m
              -> Model t    Concrete
              -> Cmd   t :@ Concrete
              -> Resp  t :@ Concrete
              -> Logic
postcondition sm@StateMachineTest{} m c r =
    toMockHandleNs (modelRefss $ after e) r .== mockResp e
  where
    e = lockstep sm m c r

symbolicResp :: StateMachineTest t m
             -> Model t Symbolic
             -> Cmd t :@ Symbolic
             -> GenSym (Resp t :@ Symbolic)
symbolicResp sm@StateMachineTest{} m c =
    At <$> nctraverse (Proxy @Typeable) (\_ _ -> FlipRef <$> genSym) resp
  where
    (resp, _mock') = step sm m c

precondition :: forall t. (NTraversable (Cmd t), All Eq (RealHandles t))
             => Model t Symbolic
             -> Cmd t :@ Symbolic
             -> Logic
precondition (Model _ (Refss hs)) (At c) =
    Boolean (M.getAll $ nfoldMap check c) .// "No undefined handles"
  where
    check :: Elem (RealHandles t) a -> FlipRef Symbolic a -> M.All
    check ix (FlipRef a) = M.All $ any (sameRef a . fst) (unRefs (hs `npAt` ix))

    -- TODO: Patch QSM
    sameRef :: Reference a Symbolic -> Reference a Symbolic -> Bool
    sameRef (QSM.Reference (QSM.Symbolic v)) (QSM.Reference (QSM.Symbolic v')) = v == v'

toStateMachine :: StateMachineTest t m
               -> StateMachine (Model t) (At (Cmd t)) m (At (Resp t))
toStateMachine sm@StateMachineTest{} = StateMachine {
      initModel     = initModel     sm
    , transition    = transition    sm
    , precondition  = precondition
    , postcondition = postcondition sm
    , generator     = generator     sm
    , shrinker      = shrinker      sm
    , semantics     = semantics     sm
    , mock          = symbolicResp  sm
    , cleanup       = cleanup       sm
    , invariant     = Nothing
    , finalCheck    = Nothing
    }

-- | Sequential test
prop_sequential :: forall t.
                   StateMachineTest t IO
                -> Maybe Int   -- ^ (Optional) minimum number of commands
                -> Property
prop_sequential sm@StateMachineTest{..} mMinSize =
    forAllCommands sm' mMinSize $ \cmds ->
      monadicIO $ do
        (hist, _model, res, _prop) <- runCommands sm' cmds
        prettyCommands sm' hist
          $ tabulate "Tags" (map show $ tagCmds cmds)
          $ res === Ok
  where
    sm' = toStateMachine sm

    tagCmds :: QSM.Commands (At (Cmd t)) (At (Resp t)) -> [Tag t]
    tagCmds = tag . map (fromLabelEvent sm) . Label.execCmds sm'

-- | Parallel test
--
-- NOTE: This currently does not do labelling.
prop_parallel :: StateMachineTest t IO
              -> Maybe Int   -- ^ (Optional) minimum number of commands
              -> Property
prop_parallel sm@StateMachineTest{} mMinSize =
    forAllParallelCommands sm' mMinSize $ \cmds ->
      monadicIO $ do
        hist <- runParallelCommands sm' cmds
        -- TODO: Ideally we would tag here as well, but unlike 'prettyCommands',
        -- 'prettyParallelCommands' does not give us a hook to specify a
        -- 'Property', so this would require a change to the QSM core.
        prettyParallelCommands cmds hist
  where
    sm' = toStateMachine sm

{-------------------------------------------------------------------------------
  Labelling
-------------------------------------------------------------------------------}

-- | Translate QSM's 'Label.Event' into our 'Event'
--
-- The QSM 'Label.Event' is purely in terms of symbolic references. In order to
-- construct our 'Event' from this, we need to reconstruct the mock response.
-- We can do this, because we maintain a mapping between references and mock
-- handles in the model, irrespective of whether those references are symbolic
-- (as here) or concrete (during test execution). We can therefore apply this
-- mapping, and re-compute the mock response.
--
-- We could use 'lockstep' instead of 'step', but this would recompute the
-- new state, which is not necessary.
--
-- NOTE: This forgets the symbolic response in favour of the mock response.
-- This seems more in line with what we do elsewhere in the lockstep
-- infrastructure, but we could conceivably return both.
fromLabelEvent :: StateMachineTest t m
               -> Label.Event (Model t) (At (Cmd t)) (At (Resp t)) Symbolic
               -> Event t Symbolic
fromLabelEvent sm Label.Event{..} = Event{
      before   = eventBefore
    , cmd      = eventCmd
    , after    = eventAfter
    , mockResp = resp'
    }
  where
    (resp', _st') = step sm eventBefore eventCmd

-- | Show minimal examples for each of the generated tags.
--
-- This is the analogue of 'Test.StateMachine.showLabelledExamples''.
-- See also 'showLabelledExamples'.
showLabelledExamples' :: StateMachineTest t m
                      -> Maybe Int
                      -- ^ Seed
                      -> Int
                      -- ^ Number of tests to run to find examples
                      -> (Tag t -> Bool)
                      -- ^ Tag filter (can be @const True@)
                      -> IO ()
showLabelledExamples' sm@StateMachineTest{..} mReplay numTests =
    Seq.showLabelledExamples'
      (toStateMachine sm)
      mReplay
      numTests
      (tag . map (fromLabelEvent sm))

-- | Simplified form of 'showLabelledExamples''
showLabelledExamples :: StateMachineTest t m -> IO ()
showLabelledExamples sm@StateMachineTest{..} =
    Seq.showLabelledExamples
      (toStateMachine sm)
      (tag . map (fromLabelEvent sm))

{-------------------------------------------------------------------------------
  Rank2 instances
-------------------------------------------------------------------------------}

instance (NTraversable (Cmd t), SListI (RealHandles t))
      => Rank2.Functor (At (Cmd t)) where
  fmap :: forall p q. (forall a. p a -> q a) -> At (Cmd t) p -> At (Cmd t) q
  fmap f (At cmd) = At $ nfmap (const f') cmd
    where
      f' :: FlipRef p a -> FlipRef q a
      f' = FlipRef . Rank2.fmap f . unFlipRef

instance (NTraversable (Cmd t), SListI (RealHandles t))
      => Rank2.Foldable (At (Cmd t)) where
  foldMap :: forall p m. Monoid m => (forall a. p a -> m) -> At (Cmd t) p -> m
  foldMap f (At cmd) = nfoldMap (const f') cmd
    where
      f' :: FlipRef p a -> m
      f' = Rank2.foldMap f . unFlipRef

instance (NTraversable (Cmd t), SListI (RealHandles t))
      => Rank2.Traversable (At (Cmd t)) where
  traverse :: forall f p q. Applicative f
           => (forall a. p a -> f (q a)) -> At (Cmd t) p -> f (At (Cmd t) q)
  traverse f (At cmd) = At <$> ntraverse (const f') cmd
    where
      f' :: FlipRef p a -> f (FlipRef q a)
      f' = fmap FlipRef . Rank2.traverse f . unFlipRef

instance (NTraversable (Resp t), SListI (RealHandles t))
      => Rank2.Functor (At (Resp t)) where
  fmap :: forall p q. (forall a. p a -> q a) -> At (Resp t) p -> At (Resp t) q
  fmap f (At cmd) = At $ nfmap (const f') cmd
    where
      f' :: FlipRef p a -> FlipRef q a
      f' = FlipRef . Rank2.fmap f . unFlipRef

instance (NTraversable (Resp t), SListI (RealHandles t))
      => Rank2.Foldable (At (Resp t)) where
  foldMap :: forall p m. Monoid m => (forall a. p a -> m) -> At (Resp t) p -> m
  foldMap f (At cmd) = nfoldMap (const f') cmd
    where
      f' :: FlipRef p a -> m
      f' = Rank2.foldMap f . unFlipRef

instance (NTraversable (Resp t), SListI (RealHandles t))
      => Rank2.Traversable (At (Resp t)) where
  traverse :: forall f p q. Applicative f
           => (forall a. p a -> f (q a)) -> At (Resp t) p -> f (At (Resp t) q)
  traverse f (At cmd) = At <$> ntraverse (const f') cmd
    where
      f' :: FlipRef p a -> f (FlipRef q a)
      f' = fmap FlipRef . Rank2.traverse f . unFlipRef

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

(!) :: Eq k => [(k, a)] -> k -> a
env ! r = fromJust (lookup r env)

zipHandles :: SListI (RealHandles t)
           => NP ([] :.: FlipRef r)    (RealHandles t)
           -> NP ([] :.: MockHandleN t) (RealHandles t)
           -> Refss t r
zipHandles = \real mock -> Refss $ hzipWith zip' real mock
  where
    zip' :: (:.:) [] (FlipRef r) a -> (:.:) [] (MockHandleN t) a -> Refs t r a
    zip' (Comp real) (Comp mock) = Refs $ zip (map unFlipRef real) mock
