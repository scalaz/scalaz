---
layout: docs
title:  "These"
---

# These

`These` (aliased as `\&/`) is an inclusive "or" of two scalaz.data types, as opposed to
`\/`, which is an exclusive "or". `These[A, B]` is isomorphic to
`A \/ B \/ (A, B)`, but is significantly easier to use and has different
scalaz.data.typeclass instances.

```tut:silent
import scalaz._
import Scalaz._
```

## Basics

A value of type `A \&/ B` can be constructed in one of three ways:

```tut
type IntAndOrString = Int \&/ String
val anInt: IntAndOrString = This(123)
val aString: IntAndOrString = That("a")
val both: IntAndOrString = Both(456, "bcd")
```

and destructured by pattern matching:

```tut
(anInt, aString, both) match {
  case (This(int1), That(str1), Both(int2, str2)) => (int1, str1, int2, str2)
  case _ => ???
}
```

There is also a `fold` method which is equivalent to pattern matching:

```tut
def myFold(t: IntAndOrString) = t.fold(_ => "This")(_ => "That")((_, _) => "Both")
(myFold(anInt), myFold(aString), myFold(both))
```

## Instances

### Bifunctor

`These` is a functor in both of its type parameters:

```tut
(anInt.lmap(_ + 1), aString.lmap(_ + 1), both.lmap(_ + 1))

(anInt.rmap(_ + "-ish"), aString.rmap(_ + "-ish"), both.rmap(_ + "-ish"))
```

Its `Functor` instance maps on the right:

```tut
both.map(_ + " and others")
```

### Monad

`These` is a monad if `A` is a `Semigroup`. `pure` is `That`, and `flatMap`
collects `A` values in `This` or `Both` cases using the semigroupal `append`.

```tut
import scalaz.typeclass.SemigroupClass

implicit val intSemigroup: Semigroup[Int] = instanceOf[SemigroupClass[Int]](_ + _)
both.flatMap((s: String) => Both(789, s + s))
aString.flatMap(_ => anInt)
```

### Semigroup

`These` is a `Semigroup` if both `A` and `B` are.

```tut
anInt append aString
anInt append anInt
both append anInt
both append both
```
Values can also be appended on either side:

```tut
anInt lappend 2
aString rappend ", fool"
both lappend 1000
```


## Other functions:

`swap` swaps `This` and `That` (and therefore also swaps the type arguments):

```tut
anInt.swap
both.swap
```

`thisSide` and `thatSide` extract a value from either side as well as `Both`:

```tut
anInt.thisSide
anInt.thatSide
both.thisSide
```
