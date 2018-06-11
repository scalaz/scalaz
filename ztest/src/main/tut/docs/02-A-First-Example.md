---
layout: docs
title: A First Example
---

# {{ page.title }}

So, let’s start with a simple version of a pure test suite, using the
most basic suite type provided. We'll be using `testz-core`,
`testz-runner`, and `testz-stdlib`.

It's provided in `testz.stdlib.PureSuite`.

```tut:silent
import testz.{PureSuite, Test}
import testz.stdlib.assertEqual
import scala.concurrent.ExecutionContext.global

final class MathTests extends PureSuite {
  def test[T](test: Test[Function0, T]): T = {
    test.section("math must")(
      test("say 1 + 1 == 2") { () =>
        assertEqual(1 + 1, 2)
      }
    )
  }
}
```

To run this type of test suite using the default testz runner, just
call `.run` with an `ExecutionContext`. The global one is fine,
usually.

```tut:book
new MathTests().run(global)
```

I went through a lot there; let's dissect that.

```tut:silent
import testz.{PureSuite, Test}
import testz.stdlib.assertEqual
import scala.concurrent.ExecutionContext.global
```

Here I import `Test[F[_], T]`, the type of test harnesses in testz.
Conventionally, test suites are written to extend a test suite class
with an abstract method that takes a `Test` as a parameter.

I also import `assertEqual` from `testz.stdlib`; assertions are just
lists of errors in testz. `assertEqual` returns an empty list of
errors if the two arguments are equal; otherwise it returns a single
error.

`PureSuite` is the test suite class I'm using. The type of test
harness it uses is a `Test[Function0, T]` for all `T`, and it returns a
`T` at the end. The idea behind this is for the test code to be unaware
of what the type `T` will be. So the only way it can return a `T` is to
use the test harness.

`Function0` (`() => ?`) is then used to wrap assertions so that they
can be computed on demand; it keeps the harness from evaluating all
assertions immediately.

```scala
final class MathTests extends PureSuite {
```

We're making a test suite class called `MathTests` extending the suite
type `PureSuite`; `MathTests` is what will show up in the output while
running the suite. This is pretty normal for a Scala test framework,
but note that this is a class and not an object. testz encourages you
not to use singleton objects as test suites. Using objects will prevent
fields of the test suites from being reachable for garbage collection
while tests run. Instead, use a class to keep your working set small
during the run.

```scala
def test[T](test: Test[Function0, T]): T = {
```

Here we define a method from `PureSuite` which we will use to define our tests.
Note that the type of the `test` method *entirely depends* on the suite
type. Any suite type could have give it any signature. You can write
your own suite type and give it any signature you want; you don't even
need to use testz's `Test` test harness type.

```scala
test.section("math must")(
```

Declaring a test section. Takes varargs parameters, of type `T`.
Returns a `T`. The only way other than `test.section` to get a `T`
(when it's abstract) is `test.apply`.

```scala
test("say 1 + 1 == 2") { () =>
```

And here's a test definition, using `test.apply`.
The first parameter is the name of the test. The parameter in the
second (curried) parameter list is a function `() => List[TestResult]`.

```scala
assertEqual(1 + 1, 2)
```

Here's the only assertion we've got. Looks fairly self-explanatory.
It'll give you a `List[TestResult]` which contains an error if the two
arguments aren't equal using `==`.

And we're done.
