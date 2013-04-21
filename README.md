# Scalaz

Scalaz is a Scala library for functional programming.

It provides purely functional data structures to complement those from the Scala standard library.
It defines a set of foundational type classes (e.g. `Functor`, `Monad`) and corresponding instances for
a large number of data structures.

[![Build Status](https://secure.travis-ci.org/scalaz/scalaz.png?branch=series/7.0.x)](http://travis-ci.org/scalaz/scalaz)

This is the branch for the 7.0.x release series. For general information about changes in version 7,
consult the documentation in the `scalaz-seven` branch or our [wiki](https://github.com/scalaz/scalaz/wiki).

## Quick Start

```scala
import scalaz._
import std.option._, std.list._ // functions and type class instances for Option and List

scala> Apply[Option].apply2(some(1), some(2))((a, b) => a + b)
res0: Option[Int] = Some(3)

scala> Traverse[List].traverse(List(1, 2, 3))(i => some(i))
res1: Option[List[Int]] = Some(List(1, 2, 3))
```

Use of the `Ops` classes, defined under `scalaz.syntax`.

```scala
import scalaz._
import std.list._ // type class instances for List
import syntax.bind._ // syntax for the Bind type class (and its parents)

scala> List(List(1)).join
res0: List[Int] = List(1)

scala> List(true, false).ifM(List(0, 1), List(2, 3))
res1: List[Int] = List(0, 1, 2, 3)
```

We've gone to great lengths to give you an *a-la-carte* importing experience, but if you prefer an all-you-can-eat
buffet, you're in luck:

```scala
import scalaz._
import Scalaz._

scala> NonEmptyList(1, 2, 3).cojoin
res0: scalaz.NonEmptyList[scalaz.NonEmptyList[Int]] = NonEmptyList(NonEmptyList(1, 2, 3), NonEmptyList(2, 3), NonEmptyList(3))

scala> 1.node(2.leaf, 3.node(4.leaf))
res1: scalaz.Tree[Int] = <tree>

scala> List(some(1), none).suml
res2: Option[Int] = Some(1)
```
