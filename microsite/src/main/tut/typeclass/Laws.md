---
layout: docs
title:  "Laws"
---

# Laws [![GitHub](../img/github.png)]

Laws accompany most type classes in scalaz.

Laws restrict implementations of type classes to satisfy
useful properties.

Laws in scalaz are written in a way which is generic over testing libraries.

Before we get to the examples, here are our imports:

```tut:silent
import scalaz._, tc._, data._, Scalaz._, laws._, tests.z._
import testz._
```

For an example, let's look at the `Monoid` left identity law.

```tut
  def leftIdentity[A, T](in: A)(assert: (A, A) => T)(implicit A: Monoid[A]): T =
    assert(in, A.mappend(A.mempty, in))
```

If you're not familiar, this law is:
`mappend(mempty, a) = a`.

`leftIdentity` abstains from choosing its own test data type
with the type parameter `A` and also abstains from choosing
an assertion type `T`. `assert` is passed in because many types
cannot be checked for equality, and because testing libraries provide
assertions ("matchers") which you can pass as `leftIdentity`'s
`assert` parameter.

A more complicated example, the `Semigroup` associativity law:

```tut
  def assoc[A, T](fst: A, snd: A, thd: A)(assert: (A, A) => T)(implicit A: Semigroup[A]): T = {
    import A.mappend
    assert(
      mappend(fst, mappend(snd, thd)),
      mappend(mappend(fst, snd), thd)
    )
  }
```

# Testing

Simple tests over laws are typically written substituting `assert` with something that checks
"equality" of some kind, and passing in different arguments under test depending on the types
being tested.

Law testing in Scalaz is currently done with testz as a testing library.

Here's an example of the `leftIdentity` law being tested for `IList`, with testz:

```tut
val lists = List(
  IList.empty[Int],
  IList.cons(20, IList.empty),
  IList.cons(20, IList.cons(30, IList.empty))
)

def tests[T](harness: Harness[T]): T =
  harness.test("mappend left identity") { () =>
    lists.foldMap {
      MonoidLaws.leftIdentity(_)(assertEqual[IList[Int]])
    }
  }
```

To sum up: using a test `Harness`, register a single test called
"mappend left identity" which takes every `IList` inside `lists`
and runs the law against it, using `assertEqual[IList[Int]]` to compare
the results for equality.

Law testing in Scalaz does not rely on random data:
laws are used with exactly the data that matters for
that particular interface and implementation.

Random testing is useful in finding some initial test cases, but doesn't
meaningfully explore the space of useful cases most of the time.

Random testing involves a lot of incredibly careful
math to get even a uniform distribution between meaningful test
cases, and if you have something like 200 meaningful test cases,
test against them directly instead of choosing 200 random
examples from them.
