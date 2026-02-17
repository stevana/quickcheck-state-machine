#### 0.10.3 (2026-02-12)

* Bump upper bound of QuickCheck for 2.17.
* Bump upper bound of random for 1.3.
* Bump upper bound of containers for 0.8.
* Bump upper bound of time for 1.15.

#### 0.10.2 (2025-07-17)

* Bump upper bound of QuickCheck to allow 2.16.

#### 0.10.1 (2024-09-05)

* Bump upper bounds of outdated dependencies.

* Add ghc-9.10.1 to the CI build matrix.

#### 0.10.0 (2024-05-28)

* Revert `WithSetup` variants that were introduced in 0.7.2. Due to the way
  repetitions are done now, there is no need for these variants anymore (PR #43);

* Delete `run[N]ParallelCommandsNTimes` in favor of the new
  `forAll[N]ParallelCommandsNTimes` as repetitions now happen at the `forAll`
  level (PR #43);

* Rename `forall` to `forAll` as it will be a reserved word starting
  in GHC 9.10.1 (see [GHC proposal 281](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst)) (PR #49);

* Replace `ansi-wl-pprint` with `prettyprinter` because `ansi-wl-pprint` was
  deprecated in favour of `prettyprinter` (PR #49).


#### 0.9.0 (2024-01-16)

* Split the package into the machinery that abstracts over whichever diffing
  implementation is in use (via the `CanDiff` class), and the implementation of
  that class via the vendored tree-diff. See the
  [README](https://github.com/stevana/quickcheck-state-machine#using-quickcheck-state-machine-as-a-dependency)
  for more details on how to depend on either.

* Support building with ghc-9.8.1.

#### 0.8.0 (2023-11-17)

* BREAKING CHANGE: Remove `markov-chain-usage-model` dependency and the related
  `Test.StateMachine.Markov` module. The tests of the dependency started
  failing:
  https://github.com/advancedtelematic/markov-chain-usage-model/issues/44 , and
  I don't think anyone is using that functionality anyway, so I decided to
  remove it (please let me know if this breaks your tests);

* Disable `ProcessRegistry` test for now, the generator needs to be migrated
  away from using the removed `Markov` module;

* Remove RQLite test as it was causing build issues with newer versions of GHC;

* Add support for newer versions of GHC (9.2.8, 9.4.7, and 9.6.3);

* Remove support for GHC 8.4.4, we need QuantifiedConstraints to build latest
  version of servant;

* Remove support for GHC 8.6.5 (released 23rd April 2019);

* Remove stack support;

* Add nix support;

* Add back CI support via GitHub Actions (remove old travis config);

* Remove the tree-diff dependency, and copy in the relevant bits that we need
  from the 0.0.2.1 version instead. The reason for this is that after that
  version the license was changed from BSD to GPL and pinning the dependency to
  that version doesn't compile with newer GHC versions, by inlining tree-diff we
  are in control of its dependecy bounds (and can thus make it compile with
  newer versions of GHC).

#### 0.7.3 (2023-6-1)

  * Fix compatibility with GHC 9.6 (PR #20, thanks @erikd);

  * Fix printing of boxed diagrams of parallel properties (PR #21);

  * Parallel properties will now show a message indicating what looks like it
    could be the cause of the property failing (PR #21).

#### 0.7.2 (2023-4-20)

  * Fix compatibility with GHC 9.0 and 9.2 (PR #7, thanks @edsko);

  * Various documentation improvements (PR #9, #10 and #13, thanks @Jasagredo);

  * Introduce new `runXCommandsXWithSetup` which allow for monadic
    initialization of the state machine for each test case execution (PR #12,
    thanks @Jasagredo).

#### 0.7.1 (2021-8-17)

  * Update links and references from the old archived repo at the
    `advancedtelematic` Github organisation to the active fork of the repo at
    the Github user `stevana`.

#### 0.7.0 (2020-3-17)

  * Add Stack resolver lts-15 and drop lts-11;

  * High-level interface to the state machine API (PR #355) -- this
    captures the patterns described in
    http://www.well-typed.com/blog/2019/01/qsm-in-depth/ as a proper
    Haskell abstraction;

  * Experimental support for Markov chain-base command generation and
    reliability calculations;

  * forAllParallelCommands now gets another argument with type Maybe
    Int, indicating the minimum number of commands we want a test case
    to have. `Nothing` provides old functionality;

  * Fixed a bug in the parallel case for the mock function (PR #348) and
    other bugs related to references in the parallel case;

  * Add a new field in the StateMachine called cleanup. This function
    can be used to ensure resources are cleaned between tests.
    `noCleanup` can be used to achieve the older functionality;

  * Improved labelling;

  * Generalize parallelism, so that more than two threads can be used
    (PR #324);

  * Option to print dot visualisation of failed examples;

  * Handle exceptions better and provide better output.

#### 0.6.0 (2019-1-15)

  This is a breaking release. See mentioned PRs for how to upgrade your code,
  and feel free to open an issue if anything isn't clear.

  * Generalise shrinking so that it might depend on the model (PR #263);

  * Drop support for GHC 8.0.* or older, by requiring base >= 4.10 (PR #267). If
    you need support for older GHC versions, open a ticket;

  * Use Stack resolver lts-13 as default (PR #261);

  * Generalise the `GConName` type class to make it possible to use it for
    commands that cannot have `Generic1` instances. Also rename the type class
    to `CommandNames` (PR #259).

#### 0.5.0 (2019-1-4)

  The first and third item in the below list might break things, have a look at
  the diffs of the PRs on how to fix your code (feel free to open an issue if it
  isn't clear).

  * Allow the user to explicitly stop the generation of commands (PR #256);
  * Fix shrinking bug (PR #255);
  * Replace MonadBaseControl IO with MonadUnliftIO (PR #252);
  * Check if the pre-condition holds before executing an action (PR #251).

#### 0.4.3 (2018-12-7)

  * Support QuickCheck-2.12.*;
  * Use new compact diffing of records from tree-diff library when displaying
    counterexamples;
  * Explain mock better in the README;
  * Handle exceptions more gracefully;
  * Show, possibly multiple, counterexample when parallel property fails.

#### 0.4.2 (2018-9-3)

  * Fix bug that made tests fail on systems without docker;
  * Remove some unused dependencies found by the weeder tool.

#### 0.4.1 (2018-8-31)

  * Minor fixes release:

    - Fix broken link and code in README;
    - Disable web server tests when docker isn't available (issue #222).

#### 0.4.0 (2018-8-21)

  * Major rewrite, addressing many issues:

    - The output of sequential runs now shows a diff of how the model changed in
      each step (related to issue #77);

    - The datatype of actions was renamed to commands and no longer is a GADT
      (discussed in issue #170, also makes issue #196 obsolete);

    - Commands can now return multiple references (issue #197);

    - Global invariants can now more easily be expressed (issue #200);

    - Counterexamples are now printed when post-conditions fail (issue #172).

#### 0.3.1 (2018-1-15)

  * Remove upper bounds for dependencies, to easier keep up with
    Stackage nightly.

#### 0.3.0 (2017-12-15)

  * A propositional logic module was added to help provide better
    counterexamples when pre- and post-conditions don't hold;

  * Generation of parallel programs was improved (base on
    a [comment](https://github.com/Quviq/QuickCheckExamples/issues/2) by
    Hans Svensson about how Erlang QuickCheck does it);

  * Support for semantics that might fail was added;

  * Pretty printing of counterexamples was improved.

#### 0.2.0

  * Z-inspired definition of relations and associated operations were
    added to help defining concise and showable models;

  * Template Haskell derivation of `shrink` and type classes: `Show`,
    `Constructors`, `HFunctor`, `HFoldable`, `HTraversable`;

  * New and more flexible combinators for building sequential and
    parallel properties replaced the old clunky ones;

  * Circular buffer example was added;

  * Two examples of how to test CRUD web applications were added.

#### 0.1.0

  * The API was simplified, thanks to ideas stolen from
    [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog/commit/385c92f9dd0aa7e748fc677b2eeead5e3572685f).

#### 0.0.0

  * Initial release.
