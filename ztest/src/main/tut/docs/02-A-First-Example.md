---
layout: docs
title: A First Example
---

# {{ page.title }}

So, let’s start with a simple version of a pure test suite, using the most
basic suite type provided. We'll be using `ztest-core`, `ztest-runner`, and
`ztest-stdlib`.

It's provided in `ztest.stdlib.PureSuite`.

```tut:silent
import ztest._

final class MathTests extends PureSuite {
  def doTests[G[_]: Monad](harness: Harness[Function0, G]): G[Unit] = {
    import harness._
    section("math must")(
      namedTest("say 1 + 1 === 2") { () =>
        if (1 + 1 === 2) Success()
        else Failure()
      }
    )
  }
}
```

To run this type of test suite using the default ztest runner, just
call `.run` and then `unsafePerformIO`.

```tut:book
new MathTests().run(global).unsafePerformIO
```

I went through a lot up there; let's dissect that.

```tut:silent
import ztest._
```

Here I import `Harness[F[_], G[_]]`, the type of test harnesses in ztest.
Conventionally, test suites are written to extend a test suite class
with an abstract method that takes a `Harness` as a parameter.

I also import `Success` and `Failure` from `ztest`; assertions are just values
in ztest.  errors in ztest. `assertEqual` returns an empty list of errors if
the two arguments are equal; otherwise it returns a single error.

`PureSuite` is the test suite class I'm using. The type of test harness it uses
is a `Test[Function0, T]` for all `T`, and it returns a `T` at the end. The
idea behind this is for the test code to be unaware of what the type `T` will
be. So the only way it can return a `T` is to use the test harness.

`Function0` (`() => ?`) is then used to wrap assertions so that they can be
computed on demand; it keeps the harness from evaluating all assertions
immediately.

```scala
final class MathTests extends PureSuite {
```

We're making a test suite class called `MathTests` extending the suite type
`PureSuite`; `MathTests` is what will show up in the output while running the
suite. This is pretty normal for a Scala test framework, but note that this is
a class and not an object. ztest encourages you not to use singleton objects as
test suites. Using objects will prevent fields of the test suites from being
reachable for garbage collection while tests run. Instead, use a class to keep
your working set small during the run.

```scala
def doTests[G[_]](harness: Harness[Function0, G]): G[Unit] = {
  import harness._
```

Here we define a method from `PureSuite` which we will use to define our tests.
Note that the type of the `doTests` method *entirely depends* on the suite type.
Any suite type could have give it any signature. You can write your own suite
type and give it any signature you want; you don't even need to use ztest's
`Harness` test harness type.

We also import all of the members of the harness - we're about to use them a
lot.

```scala
section("math must")(
```

Declaring a test section. Takes a parameter of type `G[A]` for any `A`. Returns a
`G[A]`. The only ways other than `section` to get a `G[_]` (when it's abstract)
are `test` and `shared`, on `Harness`.

```scala
test("say 1 + 1 == 2") { () =>
```

And here's a test definition, using `test.apply`.  The first parameter is the
name of the test. The parameter in the second (curried) parameter list is a
function `() => List[TestResult]`. We use no-argument functions here to make
sure we aren't keeping any test data around in memory.

```scala
if (1 + 1 === 2) Success()
else Failure()
```

Here's the only assertion we've got. Looks fairly self-explanatory.  It'll give
you a `TestResult` which is `Failure` if math is broken. Note that there is no
"assert-with-message" construct in ztest.

And we're done.
