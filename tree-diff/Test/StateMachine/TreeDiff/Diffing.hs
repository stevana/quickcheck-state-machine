{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Implements @CanDiff@ with our vendored version of @tree-diff@.
module Test.StateMachine.TreeDiff.Diffing () where

import           Data.SOP
import           Test.StateMachine.Diffing
import qualified Test.StateMachine.Lockstep.NAry    as NAry
import qualified Test.StateMachine.Lockstep.Simple  as Simple
import           Test.StateMachine.TreeDiff.Class
import           Test.StateMachine.TreeDiff.Expr
import           Test.StateMachine.TreeDiff.Pretty
import           Test.StateMachine.Types.References

instance ToExpr x => CanDiff x where
  type ADiff  x = Edit EditExpr
  type AnExpr x = Expr

  toDiff             = toExpr
  exprDiff         _ = Test.StateMachine.TreeDiff.Expr.exprDiff
  diffToDocCompact _ = ansiWlBgEditExprCompact
  diffToDoc        _ = ansiWlBgEditExpr
  exprToDoc        _ = ansiWlBgExpr

-- References

deriving newtype instance ToExpr Var

instance ToExpr a => ToExpr (Symbolic a) where
  toExpr (Symbolic x) = toExpr x

instance ToExpr a => ToExpr (Concrete a) where
  toExpr (Concrete x) = toExpr x

instance ToExpr (r a) => ToExpr (Reference a r)

instance ToExpr (Opaque a) where
  toExpr _ = App "Opaque" []

-- Simple

instance ToExpr (Simple.MockHandle t)
      => ToExpr (NAry.MockHandleN (Simple.Simple t) (Simple.RealHandle t)) where
  toExpr (Simple.SimpleToMock h) = toExpr h

-- NAry

deriving
  newtype
  instance (ToExpr a, ToExpr (NAry.MockHandleN t a)) => ToExpr (NAry.Refs t Concrete a)

instance All (And ToExpr (Compose ToExpr (NAry.MockHandleN t))) (NAry.RealHandles t)
      => ToExpr (NAry.Refss t Concrete) where
  toExpr = Lst
         . hcollapse
         . hcmap (Proxy @(And ToExpr (Compose ToExpr (NAry.MockHandleN t)))) toExprOne
         . NAry.unRefss
    where
      toExprOne :: (ToExpr a, ToExpr (NAry.MockHandleN t a))
                => NAry.Refs t Concrete a -> K Expr a
      toExprOne = K . toExpr

instance ( ToExpr (NAry.MockState t)
         , All (And ToExpr (Compose ToExpr (NAry.MockHandleN t))) (NAry.RealHandles t)
         ) => ToExpr (NAry.Model t Concrete)
