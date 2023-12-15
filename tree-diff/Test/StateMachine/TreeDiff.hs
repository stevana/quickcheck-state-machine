-- | Diffing of (expression) trees.
--
-- Diffing arbitrary Haskell data. First we convert values to untyped
-- haskell-like expression 'Expr' using generically derivable 'ToExpr' class.
-- Then we can diff two 'Expr' values.
-- The conversion and diffing is done by 'ediff' function.
-- See type and function haddocks for an examples.
--
-- Interesting modules:
--
-- * "Data.TreeDiff.Class" for a 'ToExpr' class and 'ediff' utility.
--
module Test.StateMachine.TreeDiff (
    module Test.StateMachine.TreeDiff.Expr,
    module Test.StateMachine.TreeDiff.Class,
    module Test.StateMachine.TreeDiff.Pretty,
    ) where

import           Test.StateMachine.TreeDiff.Class
import           Test.StateMachine.TreeDiff.Diffing
                   ()
import           Test.StateMachine.TreeDiff.Expr
import           Test.StateMachine.TreeDiff.Pretty
