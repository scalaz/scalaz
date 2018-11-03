---
layout: scalaz
module: base
section: typeclass
source: tc/semigroup.scala
title:  "Semigroup"
---

*A semigroup is an algebraic structure consisting of a set and an associative binary operation.*

A semigroup instance needs to satisfy the following law:

- Associativity: `append(x, append(y, z)) === append(append(x, y), z)`

**Typical imports**

```tut:silent
import scalaz.tc._
import scalaz.Scalaz._
```

# Instance declaration

```tut
implicit val intSemigroup: Semigroup[Int] = instanceOf(new SemigroupClass[Int] {
  def mappend(a1: Int, a2: => Int) = a1 + a2
})
```

# Usage

```tut
Semigroup[Int].mappend(1, 2)
1.mappend(2)
```

# Law

The sole law for a valid `Semigroup` is that `mappend` is associative:

```tut
  @inline
  def assoc[A, T](fst: A, snd: A, thd: A)(assert: (A, A) => T)(implicit A: Semigroup[A]): T = {
    import A.mappend
    assert(
      mappend(fst, mappend(snd, thd)),
      mappend(mappend(fst, snd), thd)
    )
  }
```
