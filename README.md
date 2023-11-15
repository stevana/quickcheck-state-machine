## quickcheck-state-machine

[![Hackage](https://img.shields.io/hackage/v/quickcheck-state-machine.svg)](https://hackage.haskell.org/package/quickcheck-state-machine)

`quickcheck-state-machine` is a Haskell library, based
on [QuickCheck](https://hackage.haskell.org/package/QuickCheck), for testing
stateful programs. The library is different from
the
[`Test.QuickCheck.Monadic`](https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck-Monadic.html) approach
in that it lets the user specify the correctness by means of a state machine
based model using pre- and post-conditions. The advantage of the state machine
approach is twofold: 1) specifying the correctness of your programs becomes less
adhoc, and 2) you get testing for race conditions for free.

The combination of state machine based model specification and property based
testing first appeard in Erlang's proprietary QuickCheck. The
`quickcheck-state-machine` library can be seen as an attempt to provide similar
functionality to Haskell's QuickCheck library.

### Example

As a first example, let's implement and test programs using mutable
references. Our implementation will be using `IORef`s, but let's start with a
representation of what actions are possible with programs using mutable
references. Our mutable references can be created, read from, written to and
incremented:

```haskell
data Command r
  = Create
  | Read      (Reference (Opaque (IORef Int)) r)
  | Write     (Reference (Opaque (IORef Int)) r) Int
  | Increment (Reference (Opaque (IORef Int)) r)

data Response r
  = Created (Reference (Opaque (IORef Int)) r)
  | ReadValue Int
  | Written
  | Incremented
```

When we generate actions we won't be able to create arbitrary `IORef`s, that's
why all uses of `IORef`s are wrapped in `Reference _ r`, where the parameter `r`
will let us use symbolic references while generating (and concrete ones when
executing).

In order to be able to show counterexamples, we need a show instance for our
actions. `IORef`s don't have a show instance, thats why we wrap them in
`Opaque`; which gives a show instance to a type that doesn't have one.

Next, we give the actual implementation of our mutable references. To make
things more interesting, we parametrise the semantics by a possible problem.

```haskell
data Bug = None | Logic | Race
  deriving Eq

semantics :: Bug -> Command Concrete -> IO (Response Concrete)
semantics bug cmd = case cmd of
  Create        -> Created     <$> (reference . Opaque <$> newIORef 0)
  Read ref      -> ReadValue   <$> readIORef  (opaque ref)
  Write ref i   -> Written     <$  writeIORef (opaque ref) i'
    where
    -- One of the problems is a bug that writes a wrong value to the
    -- reference.
      i' | bug == Logic && i `elem` [5..10] = i + 1
         | otherwise                        = i
  Increment ref -> do
    -- The other problem is that we introduce a possible race condition
    -- when incrementing.
    if bug == Race
    then do
      i <- readIORef (opaque ref)
      threadDelay =<< randomRIO (0, 5000)
      writeIORef (opaque ref) (i + 1)
    else
      atomicModifyIORef' (opaque ref) (\i -> (i + 1, ()))
    return Incremented
```

Note that above `r` is instantiated to `Concrete`, which is essentially the
identity type, so while writing the semantics we have access to real `IORef`s.

We now have an implementation, the next step is to define a model for the
implementation to be tested against. We'll use a simple map between references
and integers as a model.

```haskell
newtype Model r = Model [(Reference (Opaque (IORef Int)) r, Int)]

initModel :: Model r
initModel = Model []
```

The pre-condition of an action specifies in what context the action is
well-defined. For example, we can always create a new mutable reference, but
we can only read from references that already have been created. The
pre-conditions are used while generating programs (lists of actions).

```haskell
precondition :: Model Symbolic -> Command Symbolic -> Logic
precondition (Model m) cmd = case cmd of
  Create        -> Top
  Read  ref     -> ref `member` map fst m
  Write ref _   -> ref `member` map fst m
  Increment ref -> ref `member` map fst m
```

The transition function explains how actions change the model. Note that the
transition function is polymorphic in `r`. The reason for this is that we use
the transition function while generating and shrinking (with `r ~ Symbolic`) and
when executing (with `r ~ Concrete`) sequences of commands.

```haskell
transition :: Eq1 r => Model r -> Command r -> Response r -> Model r
transition m@(Model model) cmd resp = case (cmd, resp) of
  (Create, Created ref)        -> Model ((ref, 0) : model)
  (Read _, ReadValue _)        -> m
  (Write ref x, Written)       -> Model (update ref x model)
  (Increment ref, Incremented) -> case lookup ref model of
    Just i  -> Model (update ref (succ i) model)

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update ref i m = (ref, i) : filter ((/= ref) . fst) m
```

Post-conditions are checked after we executed an action and got a response from
the implementation (via `semantics`).

```haskell
postcondition :: Model Concrete -> Command Concrete -> Response Concrete -> Logic
postcondition (Model m) cmd resp = case (cmd, resp) of
  (Create,        Created ref) -> m' ! ref .== 0 .// "Create"
    where
      Model m' = transition (Model m) cmd resp
  (Read ref,      ReadValue v)  -> v .== m ! ref .// "Read"
  (Write _ref _x, Written)      -> Top
  (Increment _ref, Incremented) -> Top
```

Next we have to explain how to generate and shrink actions.

```haskell
generator :: Model Symbolic -> Maybe (Gen (Command Symbolic))
generator (Model []) = Just (pure Create)
generator model      = Just $ frequency
  [ (1, pure Create)
  , (4, Read  <$> elements (map fst model))
  , (4, Write <$> elements (map fst model) <*> arbitrary)
  , (4, Increment <$> elements (domain model))
  ]

shrinker :: Model Symbolic -> Command Symbolic -> [Command Symbolic]
shrinker _ (Write ref i) = [ Write ref i' | i' <- shrink i ]
shrinker _ _             = []
```

To stop the generation of new commands, e.g., when the model has reached a
terminal or error state, let `generator` return `Nothing`.

Finally, we show how to mock responses given a model.

```haskell
mock :: Model Symbolic -> Command Symbolic -> GenSym (Response Symbolic)
mock (Model m) cmd = case cmd of
  Create      -> Created   <$> genSym
  Read ref    -> ReadValue <$> pure (m ! ref)
  Write _ _   -> pure Written
  Increment _ -> pure Incremented
```

> `mock` is a hack to make it possible for responses to have multiple reference,
> and an experiment which maybe one day will let us create mocked APIs. See issue
> [#236](https://github.com/advancedtelematic/quickcheck-state-machine/issues/236)
> for further details.

Despite what is mentioned in the quoted issue, the `mock` function *will* be used
when advancing the model in the `Symbolic` layer, in particular in two places:

- when generating commands the model is advanced with a command generated by the
  model itself and the mocked response to that command (see
  `generateCommandsState`);

- when shrinking the list of commands by mocking responses to those commands
  which are used to advance the model and then preconditions for the shrinked
  commands are checked on that advanced model (see `shrinkAndValidate`).

Therefore, `mock` *must* provide responses that will make the model advance *on
par* with the implementation. Note that as responses might define fields which
are expected to be used only by `postcondition`, those might be filled with
dummy values in `mock` as the `postcondition` is called only with the `Concrete`
response from the implementation.

To be able to fit the code on a line we pack up all of them above into a
record.

```haskell
sm :: Bug -> StateMachine Model Command IO Response
sm bug = StateMachine initModel transition precondition postcondition
           Nothing generator shrinker (semantics bug) mock noCleanup
```

We can now define a sequential property as follows.

```haskell
prop_sequential :: Bug -> Property
prop_sequential bug = forAllCommands sm' Nothing $ \cmds -> monadicIO $ do
  (hist, _model, res) <- runCommands sm' cmds
  prettyCommands sm' hist (checkCommandNames cmds (res === Ok))
    where
      sm' = sm bug
```

If we run the sequential property without introducing any problems to the
semantics function, i.e. `quickCheck (prop_sequential None)`, then the property
passes. If we however introduce the logic bug problem, then it will fail with the
minimal counterexample:

```
> quickCheck (prop_sequential Logic)
*** Failed! Falsifiable (after 12 tests and 2 shrinks):
Commands
  { unCommands =
      [ Command Create [ Var 0 ]
      , Command (Write (Reference (Symbolic (Var 0))) 5) []
      , Command (Read (Reference (Symbolic (Var 0)))) []
      ]
  }

Model []

   == Create ==> Created (Reference (Concrete Opaque)) [ 0 ]

Model [+_×_ (Reference Opaque)
          0]

   == Write (Reference (Concrete Opaque)) 5 ==> Written [ 0 ]

Model [_×_ (Reference Opaque)
         -0
         +5]

   == Read (Reference (Concrete Opaque)) ==> ReadValue 6 [ 0 ]

Model [_×_ (Reference Opaque) 5]

PostconditionFailed "AnnotateC \"Read\" (PredicateC (6 :/= 5))" /= Ok
```

Recall that the bug problem causes the write of values ``i `elem` [5..10]`` to
actually write `i + 1`. Also notice how the diff of the model is displayed
between each action.

Running the sequential property with the race condition problem will not uncover
the race condition.

If we however define a parallel property as follows.

```haskell
prop_parallel :: Bug -> Property
prop_parallel bug = forAllParallelCommands sm' Nothing $ \cmds -> monadicIO $ do
  prettyParallelCommands cmds =<< runParallelCommands sm' cmds
    where
      sm' = sm bug
```

And run it using the race condition problem, then we'll find the race
condition:

```
> quickCheck (prop_parallel Race)
*** Failed! Falsifiable (after 26 tests and 6 shrinks):
ParallelCommands
  { prefix =
      Commands { unCommands = [ Command Create [ Var 0 ] ] }
  , suffixes =
      [ Pair
          { proj1 =
              Commands
                { unCommands =
                    [ Command (Increment (Reference (Symbolic (Var 0)))) []
                    , Command (Read (Reference (Symbolic (Var 0)))) []
                    ]
                }
          , proj2 =
              Commands
                { unCommands =
                    [ Command (Increment (Reference (Symbolic (Var 0)))) []
                    ]
                }
          }
      ]
  }
┌─────────────────────────────────────────────────────────────────────────────────────────────────┐
│ [Var 0] ← Create                                                                                │
│                                                         → Created (Reference (Concrete Opaque)) │
└─────────────────────────────────────────────────────────────────────────────────────────────────┘
┌──────────────────────────────────────────────┐ │
│ [] ← Increment (Reference (Concrete Opaque)) │ │
│                                              │ │ ┌──────────────────────────────────────────────┐
│                                              │ │ │ [] ← Increment (Reference (Concrete Opaque)) │
│                                              │ │ │                                → Incremented │
│                                              │ │ └──────────────────────────────────────────────┘
│                                → Incremented │ │
└──────────────────────────────────────────────┘ │
┌──────────────────────────────────────────────┐ │
│ [] ← Read (Reference (Concrete Opaque))      │ │
│                                → ReadValue 1 │ │
└──────────────────────────────────────────────┘ │

AnnotateC "Read" (PredicateC (1 :/= 2))

However, some repetitions of this sequence of commands passed. Maybe there is a race condition?
```

As we can see above, a mutable reference is first created, and then in
parallel (concurrently) we do two increments of said reference, and finally we
read the value `1` while the model expects `2`.

Recall that incrementing is implemented by first reading the reference and
then writing it, if two such actions are interleaved then one of the writes
might end up overwriting the other one -- creating the race condition.

We shall come back to this example below, but if your are impatient you can
find the full source
code
[here](https://github.com/stevana/quickcheck-state-machine/blob/master/test/MemoryReference.hs).

### How it works

The rough idea is that the user of the library is asked to provide:

  * a datatype of actions;
  * a datatype model;
  * pre- and post-conditions of the actions on the model;
  * a state transition function that given a model and an action advances the
    model to its next state;
  * a way to generate and shrink actions;
  * semantics for executing the actions.

The library then gives back a bunch of combinators that let you define a
sequential and a parallel property.

#### Sequential property

The *sequential property* checks if the model is consistent with respect to the
semantics. The way this is done is:

  1. generate a list of actions;

  2. starting from the initial model, for each action do the the following:

       1. check that the pre-condition holds;
       2. if so, execute the action using the semantics;
       3. check if the the post-condition holds;
       4. advance the model using the transition function.

  3. If something goes wrong, shrink the initial list of actions and present a
     minimal counterexample.

#### Parallel property

The *parallel property* checks if parallel execution of the semantics can be
explained in terms of the sequential model. This is useful for trying to find
race conditions -- which normally can be tricky to test for. It works as
follows:

  1. generate a list of actions and split it in two (or more) parts:

       - a first part that will be run sequentially, called the _prefix_ (think
         of this as an initialisation bit that setups up some state);
       - a second part (the _suffix_) that will be split in sublists which will
         be run in parallel (see `parallelSafe` to understand how it determines
         that a sequence of commands can be run in parallel). More than one
         suffix can be generated, i.e. this second step can be done multiple
         times with the part of the generated list that doesn't belong to the
         prefix.

  2. initialize the state machine if necessary (see [this](#sut-initialization));

  3. execute the prefix sequentially as in the section above (checking pre- and
     post-conditions);

  4. execute the suffixes in parallel without checking pre/post-conditions
     and gather the trace (or history) of invocations and responses of each
     action;

```
                            ┌── no checks aside from ensuring no exception was thrown
                            │
                  ╭─────────┴──────────╮

                  ┌─ [C] ──┐  ┌ [F, G] ┐   ◀─╮
Commands: [A, B] ─┤        ├──┤        │     ├─ executed `concurrently`
                  └ [D, E] ┘  └ [H, I] ┘   ◀─╯

          ╰─┬──╯       ▲          ▲
            │          ╰─────┬────╯
            │                └── groups are not run in parallel
            │                    i.e [C, D, E] will run (and
            │                    finish) before F or H are
            │                    started
            │
            └── pre/postconditions and invariant checked
                executed sequentially
```

  5. if something goes wrong when executing the commands, shrink the generated
     commands and present a minimal counterexample;

  6. otherwise, try to find a possible sequential interleaving of action
     invocations and responses that respects the post-conditions. For each
     interleaving, this is done by advancing the `Concrete` model (starting at
     `initialModel`) through the sequence of pairs of invocations to `command
     Concrete` and the returned `response Concrete` emitted at step 3, and
     checking the post-condition for each pair.

  7. if no possible sequential interleaving was found, then shrink the generated
     commands and present a minimal counterexample.

The last two steps basically try to find
a [linearisation](https://en.wikipedia.org/wiki/Linearizability) of calls that
could have happend on a single thread.

Notice that step 6 above introduces a subtlety in the post-condition checks and
transitions for the `model Concrete`. Despite the system under test running in a
concurrent way, the model can still be designed to work sequentially as *it will
not be run in parallel*, it will only be advanced in a sequential way when
evaluating possible interleavings. This particularly means that the model must
be correct with respect to sequential execution before used in parallel testing.

As we cannot control the actual scheduling of the tasks, each sequence of
commands (already fixed in a concrete prefix and a concrete list of suffixes) is
actually executed several times by default, i.e. steps 2 to 7 will be executed
multiple times for the same test case expecting that the scheduling of events
varies between runs. One can further increase entropy by introducing random
`threadDelay`s in the semantic function.

#### Why is a parallel property failing?

Unless in presence of more severe bugs, parallel properties can fail because of
mainly two reasons: a race condition is happening or a logic bug is present in
the code. Taking advantage of the fact that we are repeating multiple times each
sequence of commands, we can have some insight on which one of those cases seems
to be the cause.

The parallel counterexample will show one of the following messages:

- `However, some repetitions of this sequence of commands passed. Maybe there is
  a race condition?`: In this case as some parallel executions of a given
  sequence of commands have passed, it seems that the test outcome is being
  affected by a race condition or a non-deterministic error. This message is
  accurate in the sense that a logic bug will not result in some repetitions
  succeeding.
- `And all repetitions of this sequence of commands failed. Maybe there is a
  logic bug? Try with more repetitions to be sure that it happens consistently`:
  In this case, one of two things can happen. Either there is a logic bug that
  is triggered always (which is what the message suggest) or we were just super
  unlucky (or super lucky, as we found an error) in this run and a race
  condition manifested in all runs. In order to rule out this last case, one can
  run the tests with more repetitions, which if the problem is that there is
  indeed a race condition, should result in the other message being printed
  instead.

#### SUT initialization

Some tests might require an environment that is used by the SUT to perform its
actions, for example some mutable variable. In these cases, the environment
should be isolated from other executions, and in parallel testing each sequence
of commands is executed several times as noted in the previous paragraph. The
way to ensure that the environment is not shared among those repetitions is by
defining the state machine inside a monadic action and use the
`runXCommandsXWithSetup` variants of the functions:

``` haskell

semantics :: Env -> Command Concrete -> m (Response Concrete)
semantics = ...

sm :: m (StateMachine Model Command m Response)
sm = do
  env <- initEnv {- initialize the environment -}
  pure $ StateMachine {
    ...
    , QSM.semantics = semantics env
  }

smUnused :: StateMachine Model Command m Response
smUnused = StateMachine {
  ...
  , QSM.semantics = error "SUT must not be used during command generation or shrinking"
}

prop = forAllParallelCommands smUnused Nothing $ \cmds -> monadicIO $
  prettyParallelCommands cmds =<< runParallelCommandsWithSetup sm cmds
```

This will ensure that each execution of the testcase initializes the environment
as a first step.

Note however that when running **sequential properties**, each test case is only
executed once, therefore these two are completely equivalent:

``` haskell
sm :: m (StateMachine Model Command m Response)
sm = do
  env <- initEnv {- initialize the environment -}
  pure $ StateMachine {
    ...
    , QSM.semantics = semantics env
  }

smUnused :: StateMachine Model Command m Response
smUnused = StateMachine {
  ...
  , QSM.semantics = error "SUT must not be used during command generation or shrinking"
}

prop = forAllCommands smUnused $ \cmds -> monadicIO $
  (hist, _model, res) <- runCommandsWithSetup sm cmds
  prettyCommands smUnused hist (checkCommandNames cmds (res === Ok))
```

versus

``` haskell
sm :: Env -> StateMachine Model Command m Response
sm env = StateMachine {
    ...
    , QSM.semantics = semantics env
  }

smUnused :: StateMachine Model Command m Response
smUnused = StateMachine {
  ...
  , QSM.semantics = error "SUT must not be used during command generation or shrinking"
}

prop = forAllCommands smUnused $ \cmds -> monadicIO $
  env <- initEnv {- initialize the environment -}
  (hist, _model, res) <- runCommands (sm env) cmds
  prettyCommands smUnused hist (checkCommandNames cmds (res === Ok))
```

### More examples

Here are some more examples to get you started:

  * The water jug problem from *Die Hard 3* -- this is a
    simple
    [example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/DieHard.hs) of
    a specification where we use the sequential property to find a solution
    (counterexample) to a puzzle from an action movie. Note that this example
    has no meaningful semantics, we merely model-check. It might be helpful to
    compare the solution to the
    Hedgehog
    [solution](https://clrnd.com.ar/posts/2017-04-21-the-water-jug-problem-in-hedgehog.html) and
    the
    TLA+
    [solution](https://github.com/tlaplus/Examples/blob/master/specifications/DieHard/DieHard.tla);

  * The Tower of Hanoi puzzle -- this
    [example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/Hanoi.hs) uses
    property based testing in a very similar manner to the
    Die Hard [example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/DieHard.hs)
    to find a solution to the classic [Tower of Hanoi puzzle](https://en.wikipedia.org/wiki/Tower_of_Hanoi);

  * Mutable
    reference
    [example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/MemoryReference.hs) --
    this is a bigger example that shows both how the sequential property can
    find normal bugs, and how the parallel property can find race conditions;

  * Circular buffer
    [example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/CircularBuffer.hs)
    -- another example that shows how the sequential property can find help find
    different kind of bugs. This example is borrowed from the paper *Testing the
    Hard Stuff and Staying Sane*
    [[PDF](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf),
    [video](https://www.youtube.com/watch?v=zi0rHwfiX1Q)]. For a more direct
    translation from the paper, see the following
    [variant](https://github.com/polux/qsm-ffi-demo) which uses the C FFI;

  * The union-find
    [example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/UnionFind.hs)
    -- an imperative implementation of the union-find algorithm. It could be
    useful to compare the solution to the one that appears in the paper *Testing
    Monadic Code with QuickCheck*
    [[PS](https://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps)], which the
    [`Test.QuickCheck.Monadic`](https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck-Monadic.html)
    module is based on;

  * Ticket
    dispenser
    [example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/TicketDispenser.hs) --
    a simple example where the parallel property is used once again to find a
    race condition. The semantics in this example uses a simple database file
    that needs to be setup and cleaned up. This example also appears in the
    *Testing a Database for Race Conditions with QuickCheck* and *Testing the
    Hard Stuff and Staying
    Sane*
    [[PDF](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf),
    [video](https://www.youtube.com/watch?v=zi0rHwfiX1Q)] papers;

  * CRUD webserver where create returns unique
    ids
    [example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/CrudWebserverDb.hs) --
    create, read, update and delete users in a postgres database on a webserver
    using an API written
    using [Servant](https://github.com/haskell-servant/servant). Creating a user
    will return a unique id, which subsequent reads, updates, and deletes need
    to use. In this example, unlike in the last one, the server is setup and
    torn down once per property rather than generate program;

  * Bookstore [example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/Bookstore.hs)
    -- another database application, that uses simple SQL queries to manage a bookstore.
    It is based on a
    [case study](https://propertesting.com/book_case_study_stateful_properties_with_a_bookstore.html)
    in Erlang from online version of Fred Hebert's [PropEr Testing](https://propertesting.com/toc.html)
    book;

  * Process registry
    [example](https://github.com/stevana/quickcheck-state-machine/blob/master/test/ProcessRegistry.hs)
    -- an example often featured in the Erlang QuickCheck papers. This example
    shows how one can tag the specification with which requirements are covered
    and then generate (minimal) examples of test cases that cover each
    requirement, as shown in the *How well are your requirements tested?*
    [[PDF](https://publications.lib.chalmers.se/records/fulltext/232552/local_232552.pdf)]
    and *Understanding Formal Specifications through Good Examples*
    [[PDF](https://doi.org/10.1145/3239332.3242763),
    [video](https://www.youtube.com/watch?v=w2fin2V83e8)] papers.

All properties from the examples can be found in the
[`Spec`](https://github.com/stevana/quickcheck-state-machine/blob/master/test/Spec.hs)
module located in the
[`test`](https://github.com/stevana/quickcheck-state-machine/tree/master/test)
directory.

To get a better feel for the examples it might be helpful to `git clone` this
repo, `cd` into it, fire up `cabal repl test`, load the different examples,
e.g. `:l test/CrudWebserverDb.hs`, and run the different properties
interactively.

### Real world examples

More examples from the "real world":

  * IOHK are using a state machine models in several
    [places](https://github.com/search?l=Haskell&q=org%3Ainput-output-hk+Test.StateMachine&type=Code).
    For example
    [here](https://github.com/input-output-hk/fs-sim/blob/main/fs-sim/test/Test/System/FS/StateMachine.hs)
    is a test of a mock file system that they in turn use to simulate file
    system errors when testing a blockchain database. The following blog
    [post](http://www.well-typed.com/blog/2019/01/qsm-in-depth/) describes their
    tests in more detail;

  * Wire are using a state machine model to
    [test](https://github.com/wireapp/wire-server/blob/master/services/gundeck/test/unit/ThreadBudget.hs)
    the lower bound of running threads in their push notification system;

  * Adjoint's (now abandoned?) implementation of the Raft consensus algorithm,
    contains state machine
    [tests](https://github.com/stevana/raft/blob/master/test/QuickCheckStateMachine.hs)
    combined with fault injection (node and network failures).

### How to contribute

The `quickcheck-state-machine` library is still very experimental.

We would like to encourage users to try it out, and join the discussion of how
we can improve it on the issue tracker!

### See also

  * The QuickCheck bugtrack
    [issue](https://github.com/nick8325/quickcheck/issues/139) -- where the
    initial discussion about how to add state machine based testing to
    QuickCheck started;

  * John Hughes' Midlands Graduate School 2019
    [course](https://www.cse.chalmers.se/~rjmh/MGS2019/) on property-based
    testing, which covers the basics of state machine modelling and testing. It
    also contains a minimal implementation of a state machine testing library
    built on top of Haskell's QuickCheck;

  * *Finding Race Conditions in Erlang with QuickCheck and
    PULSE*
    [[PDF](https://www.cse.chalmers.se/~nicsma/papers/finding-race-conditions.pdf),
    [video](https://vimeo.com/6638041)] -- this is the first paper to describe
    how Erlang's QuickCheck works (including the parallel testing);

  * *Linearizability: a correctness condition for concurrent objects*
    [[PDF](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf), TLA+
    [formalisation](https://github.com/lorin/tla-linearizability)], this is a classic
    paper that describes the main technique of the parallel property;

  * Aphyr's blogposts about [Jepsen](https://github.com/jepsen-io/jepsen), which
    also uses the linearisability technique, and has found bugs in many
    distributed systems:

    - [Knossos: Redis and linearizability](https://aphyr.com/posts/309-knossos-redis-and-linearizability);
    - [Strong consistency models](https://aphyr.com/posts/313-strong-consistency-models);
    - [Computational techniques in Knossos](https://aphyr.com/posts/314-computational-techniques-in-knossos);
    - [Serializability, linearizability, and locality](https://aphyr.com/posts/333-serializability-linearizability-and-locality).

  * The use of state machines to model and verify properties about programs is
    quite well-established, as witnessed by several books on the subject:

      - [Specifying
        Systems](https://www.microsoft.com/en-us/research/publication/specifying-systems-the-tla-language-and-tools-for-hardware-and-software-engineers/):
        The TLA+ Language and Tools for Hardware and Software Engineers.
        Parts of this book are also presented by the author, Leslie
        Lamport, in the following video
        [course](https://lamport.azurewebsites.net/video/videos.html);

      - [Modeling in Event-B](http://www.event-b.org/abook.html): System
        and Software Engineering. Parts of this book are covered in the
        following (video) course given at Microsoft Research by the
        author, Jean-Raymond Abrial, himself:

          + [Lecture 1](https://www.youtube.com/watch?v=2GP1pJINVT4):
            introduction to modelling and Event-B (chapter 1 of the
            book) and start of "controlling cars on bridge" example
            (chapter 2);

          + [Lecture 2](https://www.youtube.com/watch?v=M8nvVaZ74wA):
            refining the "controlling cars on a bridge" example
            (sections 2.6 and 2.7);

          + [Lecture 3](https://www.youtube.com/watch?v=Y5OUtq8cdV8):
            design patterns and the "mechanical press controller"
            example (chapter 3);

          + [Lecture 4](https://www.youtube.com/watch?v=ku-lfjxM4WI):
            sorting algorithm example (chapter 15);

          + [Lecture 5](https://www.youtube.com/watch?v=C0tpgPOKAyg):
            designing sequential programs (chapter 15);

          + [Lecture 6](https://www.youtube.com/watch?v=i-GKHZAWWjU):
            status report of the hypervisor that Microsoft Research are
            developing using Event-B.

      - [Abstract State Machines](http://groups.di.unipi.it/~boerger/AsmBook/): A
        Method for High-Level System Design and Analysis.

    The books contain general advice how to model systems using state machines,
    and are hence relevant to us. For shorter texts on why state machines are
    important for modelling, see:

      - Lamport's
        [*Computation and State Machines*](https://www.microsoft.com/en-us/research/publication/computation-state-machines/);

      - Gurevich's
        [*Evolving Algebras 1993: Lipari Guide*](https://www.microsoft.com/en-us/research/publication/103-evolving-algebras-1993-lipari-guide/) and
        *Sequential Abstract State Machines Capture Sequential
        Algorithms*
        [[PDF](http://delta-apache-vm.cs.tau.ac.il/~nachumd/models/gurevich.pdf)].

  * Other similar libraries:

      - Erlang QuickCheck, [eqc](http://quviq.com/documentation/eqc/), the first
        property based testing library to have support for state machines
        (closed source);

      - The Erlang library [PropEr](https://github.com/proper-testing/proper) is
        *eqc*-inspired, open source, and has support for state
        machine [testing](http://propertesting.com/);

      - The Haskell
        library [Hedgehog](https://github.com/hedgehogqa/haskell-hedgehog), also
        has support for state machine based testing;

      - [ScalaCheck](https://scalacheck.org/), likewise has support for state
        machine
        based
        [testing](https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md) (no
        parallel property);

      - The Python
        library [Hypothesis](https://hypothesis.readthedocs.io/en/latest/), also
        has support for state machine
        based
        [testing](https://hypothesis.readthedocs.io/en/latest/stateful.html) (no
        parallel property).

### History and current status

This library was originally developed while I was working at *ATS Advanced
Telematic Systems GmbH* between 2017 and 2018. In 2018 *HERE Europe B.V*
acquired *ATS* and took over control over the
[advancedtelematic](https://github.com/advancedtelematic) GitHub organisation. I
left *HERE* in 2019 and in 2021 they archived the old
[`quickcheck-state-machine`](https://github.com/advancedtelematic/quickcheck-state-machine)
repo making it read-only -- that's when this fork was created.

I no longer use `quickcheck-state-machine` on a daily basis, and have no plans
for making any major changes. That said, I consider the library fairly feature
complete and stable and I'm happy to do minor maintenance work. I'm also happy
to help and mentor anyone willing to take on a more active development role.

### License

BSD-style (see the file LICENSE).
