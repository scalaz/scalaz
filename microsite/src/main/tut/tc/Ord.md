---
layout: scalaz
module: base
section: typeclass
source: tc/ord.scala
title:  "Ord"
---

*Ord is used for totally ordered datatypes.*

Ord builds on top of [Eq](../Eq.html) in that it adds the concept of ordering values.
A value can be "greater than", "less than" or "equal to" another value.

**Typical imports**

```tut:silent
import scalaz._
import Predef._, data._, tc._, Scalaz._
```

# Instance declaration

```tut
implicit val intOrd: Ord[Int] = instanceOf(new OrdClass[Int] {
  def comp(a: Int, b: Int): Ordering = (a, b) match {
    case (x, y) if (x < y) => LT
    case (x, y) if (x > y) => GT
    case _ => EQ
  }
})
```

# Usage

```tut
val a = 5
val b = 7
val c = 5

a comp b
a comp c
b comp c
```

# Law

The laws for a total order, in addition to the laws for [Eq](./Eq.html), are stated as:

```tut
def antisymmetry[A, T](fst: A, snd: A)(assert: Boolean => T)(implicit A: Ord[A]): T = {
  val mustBeEqual =
    ((A.<=(fst, snd)) && (A.<=(snd, fst)))
  val satisfiesAntisymmetry =
    mustBeEqual == A.equal(fst, snd)
  assert(satisfiesAntisymmetry)
}

def connex[A, T](fst: A, snd: A)(assert: Boolean => T)(implicit A: Ord[A]): T = {
  val satisfiesConnex = A.<=(fst, snd) || A.<=(snd, fst)
  assert(satisfiesConnex)
}

def transitivity[A, T](fst: A, snd: A, thd: A)(assert: Boolean => T)(implicit A: Ord[A]): T = {
  val chain = A.<=(fst, snd) && A.<=(snd, thd)
  val satisfiesTransitivity =
    if (chain) A.<=(fst, thd)
    else true
  assert(satisfiesTransitivity)
}
```
