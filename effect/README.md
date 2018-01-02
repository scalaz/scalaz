# scalaz-effect

The `scalaz.effect` package provides a general-purpose effect monad and associated abstractions for purely functional Scala applications.

The package strives to deliver on the following design goals:

 - **Principled**. A purely functional interface for effectful code with rigorous, well-defined semantics.
 - **Performant**. A low-level, highly-optimized runtime system that offers performance better than or comparable to other effect monads.
 - **Pragmatic**. The composable, orthogonal primitives necessary to build real world software, including primitives for concurrent and asynchronous programming.

# Why IO?

Effect monads like `IO` are how purely functional programs interact with the real world. Functional programmers use them to build complex, real world software without giving up the equational reasoning, composability, and type safety afforded by purely functional programming.

However, there are many practical reasons to build your programs using `IO`, including all of the following:

 * **Asynchronicity**. Like Scala's `Future`, `IO` lets you easily write asynchronous code without blocking or callbacks. Compared to `Future`, `IO` has significantly better performance and cleaner, more expressive, and more composable semantics.
 * **Composability**. Purely functional code can't be combined with impure code that has side-effects without the straightforward reasoning properties of functional programming. `IO` lets you wrap up all effects into a purely functional package that lets you build composable real world programs.
 * **Concurrency**. `IO` has all the concurrency features of `Future`, and more, based on a clean fiber concurrency model designed to scale well past the limits of native threads. Unlike other effect monads, `IO`'s concurrency primitives do not leak threads.
 * **Interruptibility**. All concurrent computations can be interrupted, in a way that still guarantees resources are cleaned up safely, allowing you to write aggressively parallel code that doesn't waste valuable resources or bring down production servers.
 * **Resource Safety**. `IO` provides composable resource-safe primitives that ensure resources like threads, sockets, and file handles are not leaked, which allows you to build long-running, robust applications. These applications will not leak resources, even in the presence of errors or interruption.
 * **Immutability**. `IO`, like Scala's immutable collection types, is an immutable data structure. All `IO` methods and functions return new `IO` values. This lets you reason about `IO` values the same way you reason about immutable collections.
 * **Reification**. `IO` reifies programs. In non-functional Scala programming, you cannot pass programs around or store them in data structures, because programs are not values. But `IO` turns your programs into ordinary values, and lets you pass them around and compose them with ease.
 * **Performance**. Although simple, synchronous `IO` programs tend to be slower than the equivalent imperative Scala, `IO` is extremely fast given all the expressive features and strong guarantees it provides. Ordinary imperative Scala could not match this level of expressivity and performance without tedious, error-prone boilerplate that no one would write in real-life.

While functional programmers *must* use `IO` (or something like it) to represent effects, nearly all programmers will find the features of `IO` help them build scalable, performant, concurrent, and leak-free applications faster and with stronger correctness guarantees than legacy techniques allow.

Use `IO` because it's simply not practical to write real-world, correct software without it.

# Usage

## Main

Your main function can extend `SafeApp`, which provides a complete runtime
system and allows your entire program to be purely functional.

```scala
import scalaz.effect.{IO, SafeApp}
import scalaz.effect.console._

object MyApp extends SafeApp {
  def run(args: IList[String]): IO[Unit] =
    for {
      _ <- putStrLn("Hello! What is your name?")
      n <- getStrLn
      _ <- putStrLn("Hello, " + n + ", good to meet you!")
    } yield ()
}
```

## Pure Values

You can lift pure values into `IO` with `IO.point`:

```scala
val liftedString: IO[String] = IO.point("Hello World")
```

The constructor uses non-strict evaluation, so the parameter will not be evaluated until when and if the `IO` action is executed at runtime.

Alternately, you can use the `IO.now` constructor for strict evaluation:

```scala
val lifted: IO[String] = IO.now("Hello World")
```

You should never use either constructor for impure code. The result of doing so is undefined, and will probably result in runtime errors.

## Impure Code

You can use the `sync` method of `IO` to import effectful synchronous code into your purely functional program:

```scala
def readFile(f: String): IO[ByteArray] = IO.sync(Bytes.readFile(f))
```

You can use the `async` method of `IO` to import effectful asynchronous code into your purely functional program:

```scala
def makeRequest(req: Request): IO[Response] =
  IO.async(cb => Http.req(req, cb))
```

## Mapping

You can change an `IO[A]` to an `IO[B]` by calling the `map` method with a function `A => B`. This lets you transform values produced by actions into other values.

```scala
val answer = IO.point(21).map(_ * 2)
```

## Chaining

You can execute two actions in sequence with the `flatMap` method. The second action may depend on the value produced by the first action.

```scala
val contacts: IO[IList[Contact]] =
  readFile("contacts.csv").flatMap((file: ByteArray) =>
    parseCsv(file).map((csv: IList[CsvRow]) =>
      csv.map(rowToContact)
    )
  )
```

You can use Scala's `for` comprehension syntax to make this type of code more compact:

```scala
val contacts: IO[IList[Contact]] =
  for {
    file <- readFile("contacts.csv")
    csv  <- parseCsv(file)
  } yield csv.map(rowToContact)
```

## Failure

You can create `IO` actions that describe failure with `IO.fail`:

```scala
val io: IO[String] = IO.fail(new Error("Oh noes!"))
```

Like all `IO` values, these are immutable values and do not actually throw any exceptions; they merely describe failure as a first-class value.

You can surface failures with `attempt`, which takes an `IO[A]` and produces an `IO[Throwable \/ A]`:

```scala
val result = openFile("data.json").attempt.map {
  case -\/ (error) => /* handle error */ ???
  case  \/-(file)  => /* handle file */ ???
}
```

You can submerge failures with `IO.absolve`, which turns an `IO[Throwable \/ A]` into an `IO[A]`:

```scala
def sqrt(io: IO[Double]): IO[Double] =
  IO.absolve(
    io.map(value =>
      if (value < 0.0) -\/(new Error("Only non-negative values!"))
      else \/-(Math.sqrt(value))
    )
  )
```

If you want to catch and recover from all types of exceptions and effectfully attempt recovery, you can use the `catchAll` method:

```scala
openFile("primary.json").catchAll(_ => openFile("backup.json"))
```

If you want to catch and recover from only some types of exceptions and effectfully attempt recovery, you can use the `catchSome` method:

```scala
openFile("primary.json").catchSome {
  case FileNotFoundException(_) => openFile("backup.json")
}
```

You can execute one action, or, if it fails, execute another action, with the `orElse` combinator:

```scala
val file = openFile("primary.json").orElse(openFile("backup.json"))
```

### Retry

There are a number of useful combinators for repeating actions until failure or success:

 * `IO.forever` &mdash; Repeats the action until the first failure.
 * `IO.retry` &mdash; Repeats the action until the first success.
 * `IO.retryN(n)` &mdash; Repeats the action until the first success, for up to the specified number of times (`n`).
 * `IO.retryFor(d)` &mdash; Repeats the action until the first success, for up to the specified amount of time (`d`).

## Brackets

Brackets are a built-in primitive that let you safely acquire and release resources.

Brackets are used for a similar purpose as try/catch/finally, only brackets work with synchronous and asynchronous actions, work seamlessly with fiber interruption, and are built on a different error model that ensures no errors are ever swallowed.

Brackets consist of an *acquire* action, a *utilize* action (which uses the acquired resource), and a *release* action.

The release action is guaranteed to be executed by the runtime system, even if the utilize action throws an exception or the executing fiber is interrupted.

```scala
openFile("data.json").bracket(closeFile(_)) { file =>
  for {
    data    <- decodeData(file)
    grouped <- groupData(data)
  } yield grouped
}
```

Brackets have compositional semantics, so if a bracket is nested inside another bracket, and the outer bracket acquires a resource, then the outer bracket's release will always be called, even if, for example, the inner bracket's release fails.

A helper method called `ensuring` provides a simpler analogue of `finally`:

```scala
val composite = action1.ensuring(cleanupAction)
```

## Fibers

To perform an action without blocking the current process, you can use fibers, which are a lightweight mechanism for concurrency.

You can `fork` any `IO[A]` to immediately yield an `IO[Fiber[A]]`. The provided `Fiber` can be used to `join` the fiber, which will resume on production of the fiber's value, or to `interrupt` the fiber with some exception.

```scala
val analyzed =
  for {
    fiber1   <- analyzeData(data).fork  // IO[Analysis]
    fiber2   <- validateData(data).fork // IO[Boolean]
    ... // Do other stuff
    valid    <- fiber2.join
    _        <- if (!valid) fiber1.interrupt(DataValidationError(data))
                else IO.unit
    analyzed <- fiber1.join
  } yield analyzed
```

On the JVM, fibers will use threads, but will not consume *unlimited* threads. Instead, fibers yield cooperatively during periods of high-contention.

```scala
def fib(n: Int): IO[Int] =
  if (n <= 1) IO.point(1)
  else for {
    fiber1 <- fib(n - 2).fork
    fiber2 <- fib(n - 1).fork
    v2     <- fiber2.join
    v1     <- fiber1.join
  } yield v1 + v2
```

Interrupting a fiber returns an action that resumes when the fiber has completed or has been interrupted and all its finalizers have been run. These semantics allow construction of programs that do not leak resources.

A more powerful variant of `fork`, called `fork0`, allows specification of a handler that will be passed any uncaught errors from the forked fiber, including all uncaught errors that occur in finalizers. If this handler is not specified, then the handler of the parent fiber will be used, recursively, up to the root handler, which can be specified in `RTS`.

### Parallelism

To execute actions in parallel, the `par` method can be used:

```scala
def bigCompute(m1: Matrix, m2: Matrix, v: Matrix): IO[Matrix] =
  for {
    t <- computeInverse(m1).par(computeInverse(m2))
    val (i1, i2) = t
    r <- applyMatrices(i1, i2, v)
  } yield r
```

The `par` combinator has resource-safe semantics. If one computation fails, the other computation will be interrupted, to prevent wasting resources.

### Racing

Two `IO` actions can be *raced*, which means they will be executed in parallel, and the value of the first action that completes successfully will be returned.

```scala
action1.race(action2)
```

The `race` combinator is resource-safe, which means that if one of the two actions returns a value, the other one will be interrupted, to prevent wasting resources.

The `race` and even `par` combinators are a specialization of a much-more powerful combinator called `raceWith`, which allows executing user-defined logic when the first of two actions succeeds.

# Performance

`scalaz.effect` has excellent performance, featuring a hand-optimized, low-level interpreter that achieves zero allocations for right-associated binds, and minimal allocations for left-associated binds.

The `benchmarks` project may be used to compare `IO` with other effect monads, including `Future` (which is not an effect monad but is included for reference), Monix `Task`, and Cats `IO`.

As of the time of this writing, `IO` is significantly faster than or at least comparable to all other purely functional solutions.

# Stack Safety

`IO` is stack-safe on infinitely recursive `flatMap` invocations, for pure values, synchronous effects, and asynchronous effects. `IO` is guaranteed to be stack-safe on repeated `map` invocations (`io.map(f1).map(f2)...map(f10000)`), for at least 10,000 repetitions.

|      |        map        |  flatMap  |
|-----:|:-----------------:|:---------:|
| sync | 10,000 iterations | unlimited |
| async| unlimited         | unlimited |

# Thread Shifting - JVM

By default, fibers make no guarantees as to which thread they execute on. They may shift between threads, especially as they execute for long periods of time.

Fibers only ever shift onto the thread pool of the runtime system, which means that by default, fibers running for a sufficiently long time will always return to the runtime system's thread pool, even when their (asynchronous) resumptions were initiated from other threads.

For performance reasons, fibers will attempt to execute on the same thread for a (configurable) minimum period, before yielding to other fibers. Fibers that resume from asynchronous callbacks will resume on the initiating thread, and continue for some time before yielding and resuming on the runtime thread pool.

These defaults help guarantee stack safety and cooperative multitasking. They can be changed in `RTS` if automatic thread shifting is not desired.

# Legal

Copyright (C) 2017 John A. De Goes. All rights reserved.
