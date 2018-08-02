---
layout: docs
section: data
title:  "These"
---

# These [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/data/these.scala)

`These` (aliased as `\&/`) is an inclusive "or" of two data types, as opposed to
[\/](./Disjunction.html), which is an exclusive "or". `These[A, B]` is isomorphic to
`A \/ B \/ (A, B)`, but is significantly easier to use and has different
typeclass instances.

# Basics

A value of type `A \&/ B` can be constructed in one of three ways:
From a single `A`, from a single `B`, or from both an `A` and a `B`.

```tut
import scalaz.data._, scalaz.Scalaz._
val anInt: List[Int] \&/ String = This(List(123))
val aString: List[Int] \&/ String = That("a")
val both: List[Int] \&/ String = Both(List(456), "bcd")
```

Being destructured by pattern matching:

```tut
import scala.Predef.???

(anInt, aString, both) match {
  case (This(int1), That(str1), Both(int2, str2)) => (int1, str1, int2, str2)
  case _ => ???
}
```

There is also a `fold` method which is equivalent to pattern matching:

```tut
def myFold(t: List[Int] \&/ String): String =
  t.fold(_ => "This", _ => "That", (_, _) => "Both")
((myFold(anInt), myFold(aString), myFold(both)))
```

# Instances

## Bifunctor

`These` is a [functor](../tc/Functor.html) in both of its type parameters:

```tut
(anInt.lmap(1 :: _), aString.lmap(1 :: _), both.lmap(1 :: _))

(anInt.rmap(_ + "-ish"), aString.rmap(_ + "-ish"), both.rmap(_ + "-ish"))
```

Its `Functor` instance maps on the right:

```tut
both.map(_ + " and others")
```

## Monad

`These` is a [monad](../tc/Monad.html) if `A` is a [Semigroup](../tc/Semigroup.html). `pure` is `That`, and `flatMap`
collects `A` values in `This` or `Both` cases using the semigroupal `mappend`.

```tut
import scalaz.tc._

// implicit val intSemigroup: Semigroup[Int] = instanceOf[SemigroupClass[Int]](_ + _)
// both.flatMap((s: String) => Both(List(789), s + s))
aString.flatMap(_ => anInt)
```

## Semigroup

`These` is a [Semigroup](../tc/Semigroup.html) if both `A` and `B` are.

```tut:silent
anInt.mappend(aString)
anInt.mappend(anInt)
both.mappend(anInt)
both.mappend(both)
```
Values can also be appended on either side:

```tut:silent
anInt.lappend(List(2))
aString.rappend(", fool")
both.lappend(List(1000))
```


# Other functions:

`swap` swaps `This` and `That` (and therefore also swaps the type arguments):

```tut:silent
anInt.swap
both.swap
```

`thisSide` and `thatSide` extract a value from either side as well as `Both`:

```tut:silent
anInt.thisSide
anInt.thatSide
both.thisSide
```
