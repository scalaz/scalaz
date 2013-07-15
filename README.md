# Scalaz

Scalaz is a Scala library for functional programming.

It provides purely functional data structures to complement those from the Scala standard library.
It defines a set of foundational type classes (e.g. `Functor`, `Monad`) and corresponding instances for
a large number of data structures.

[![Build Status](https://secure.travis-ci.org/scalaz/scalaz.png?branch=scalaz-seven)](http://travis-ci.org/scalaz/scalaz)

## Getting Scalaz

The current stable version is 7.0.2, which is cross-built against Scala 2.9.2, 2.9.3 and 2.10.x.

If you're using SBT, add the following lines to your build file:

```scala
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.2"
```

For Maven and other build tools, you can visit [search.maven.org](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.scalaz%22%20AND%20v%3A%227.0.2%22).
(This search will also list all available modules of scalaz.)

To get sample configurations, click on the version of the module you are interested in.
You can also find direct download links at the bottom of that page. Choose the file ending in `7.0.2.jar`.

Note: In some cases, Maven will warn about "multiple versions of scala libraries", e.g.

```
[WARNING]  Expected all dependencies to require Scala version: 2.10.1
[WARNING]  org.scalaz:scalaz-core_2.10:7.0.2 requires scala version: 2.10.0
```

You can safely ignore this warning, as the Scala 2.10.x versions are compatible.

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

## Changes in Version 7

Scalaz 7 represents a major reorganization of the library. We have taken a fresh look
at the challenges of encoding type classes in Scala, in particular at when and how to
employ the implicit scope.

### At a glance

* `scalaz.{concurrent, effect, iteratee}` split to separate sub-projects; `scalaz.{http, geo}` dropped.
* Refined and expanded the type class hierarchy.
* Type class instances are no longer defined in the companion objects of the type class.
  Instances for standard library types are defined under `scalaz.std`, and instances for
  Scalaz data types are defined in the companion object for those types. An instance definition
  can provide multiple type classes in a single place, which was not always possible in Scalaz 6.
* Type class instances have been organized to avoid ambiguity, a problem that arises when
  instances are dependent on other instances (for example, `Monoid[(A, B)]`)
* Use of implicit views to provide access to Scalaz functionality as extension methods
  has been been segregated to `scalaz.syntax`, and can be imported selectively, and need not
  be used at all.
* Related functions are defined in the type class trait, to support standalone
  usage of the type class. In Scalaz 6, these were defined in `Identity`, `MA`, or `MAB`.
* New data structures have been added, and existing ones generalized. A number of monad
  transformers have been provided, in some cases generalizing old data structures.

### Modularity

Scalaz has been been modularised.

* **scalaz-core**: Type class hierarchy, data structures, type class instances for the Scala and Java standard libraries,
                 implicit conversions / syntax to access these.
* **scalaz-effect**: Data structures to represent and compose IO effects in the type system.
* **scalaz-concurrent**: Actor and Promise implementation
* **scalaz-iteratee**: Experimental new Iteratee implementation
* **scalaz-typelevel**: Type-level data structures, type-safe printf
* **scalaz-xml**: Error-correcting XML parser

### Type Class Hierarchy

* Type classes form an inheritance hierarchy, as in Scalaz 6. This is convenient both at the call site and at the
  type class instance definition. At the call site, it ensures that you can call a method requiring a more general
  type class with an instance of a more specific type class:

```scala
def bar[M[_]: Functor] = ()

def foo[M[_]: Monad] = bar[M] // Monad[M] is a subtype of Functor[M]
```

* The hierarchy itself is largely the same as in Scalaz 6. However, there have been a few
 adjustments, some method signatures have been adjusted to support better standalone usage, so code depending on these will
 need to be re-worked.

### Type Class Instance Definition

* *Constructive* implicits, which create a type class instance automatically based on instances of
  all parent type classes, are removed. These led to subtle errors with ambiguous implicits, such as
  this problem with [FunctorBindApply](http://stackoverflow.com/questions/7447591/how-do-i-use-name-as-an-applicative/7448111#7448111)
* Type class instances are no longer declared in fragments in the companion objects of the type class. Instead, they
  are defined in the package `scalaz.std`, and must be imported. These instances are defined in traits which will be
  mixed together into an object for importing *en-masse*, if desired.
* A single implicit can define a number of type class instances for a type.
* A type class definition can override methods (including derived methods) for efficiency.

Here is an instance definition for `Option`. Notice that the method `map` has been overriden.

```scala
  implicit val option = new Traverse[Option] with MonadPlus[Option] {
    def point[A](a: => A) = Some(a)
    def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
    def traverseImpl[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]) =
      fa map (a => F.map(f(a))(Some(_): Option[B])) getOrElse F.point(None)
    def empty[A]: Option[A] = None
    def plus[A](a: Option[A], b: => Option[A]) = a orElse b
    def foldR[A, B](fa: Option[A], z: B)(f: (A) => (=> B) => B): B = fa match {
      case Some(a) => f(a)(z)
      case None => z
    }
  }
```

To use this, one would:

```scala
import scalaz.std.option.optionInstance
// or, importing all instances en-masse
// import scalaz.Scalaz._

val M = Monad[Option]
val oi: Option[Int] = M.point(0)
```

### Syntax

We co-opt the term *syntax* to refer to the way we allow the functionality of Scalaz to be
called in the `object.method(args)` form, which can be easier to read, and, given that type inference
in Scala flows from left-to-right, can require fewer type annotations.

* No more `Identity`, `MA`, or `MAB` from Scalaz 6.
* Syntax is segregated from rest of the library, in a sub-package `scalaz.syntax`.
* All Scalaz functionality is available *without* using the provided syntax, by directly calling methods
  on the type class or its companion object.
* Syntax is available *a-la-carte*. You can import the syntax for working with particular
  type classes where you need it. This avoids flooding the autocompletion in your IDE with
  every possible extension method. This should also help compiler performance,
  by reducing the implicit search space.
* Syntax is layered in the same way as type classes. Importing the syntax for, say, `Applicative`
  will also provide the syntax for `Apply` and `Functor`.

Syntax can be imported in two ways. Firstly, the syntax specialized for a particular instance
of a type class can be imported directly from the instance itself.

```scala
// import the type class instance
import scalaz.std.option.optionInstance

// import the implicit conversions to `MonadOps[Option, A]`, `BindOps[Option, A]`, ...
import optionInstance.monadSyntax._

val oi: Option[Option[Int]] = Some(Some(1))

// Expands to: `ToBindOps(io).join`
oi.join
```

Alternatively, the syntax can be imported for a particular type class.

```scala
// import the type class instance
import scalaz.std.option.optionInstance

// import the implicit conversions to `MonadOps[F, A]`, `BindOps[F, A]`, ...
import scalaz.syntax.monad._

val oi: Option[Option[Int]] = Some(Some(1))

// Expands to: ToBindOps(io).join
oi.join
```

For some degree of backwards compatibility with Scalaz 6, the über-import of `import scalaz.Scalaz._`
will import *all* implicit conversions that provide syntax (as well as type class instances and other
functions). However, we recommend to review usage of this and replace with more focussed imports.

### Standalone Type Class Usage

Type classes should be directly usable, without first needing to trigger implicit conversions. This might be
desirable to reduce the runtime or cognitive overhead of the pimped types, or to define your own pimped
types with a syntax of your choosing.

* The methods in type classes have been curried to maximize type inference.
* Derived methods, based on the abstract methods in a type class, are defined in the type class itself.
* Each type class companion object is fitted with a convenient `apply` method to obtain an instance of the type class.

```scala
    // Equivalent to `implicitly[Monad[Option]]`
    val O = Monad[Option]

    // `bind` is defined with two parameter sections, so that the type of `x` is inferred as `Int`.
    O.bind(Some(1))(x => Some(x * 2)

    def plus(a: Int, b: Int) = a + b

    // `Apply#lift2` is a function derived from `Apply#ap`.
    val plusOpt = O.lift2(plus)
```

### Type Class Instance Dependencies

Type class instances may depend on other instances. In simple cases, this is as straightforward as adding an implicit
parameter (or, equivalently, a context bound), to the implicit method.

```scala
  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def append(f1: Option[A], f2: => Option[A]): Option[A] = (f1, f2) match {
      case (Some(a1), Some(a2)) => Some(Semigroup[A].append(a1, a2))
      case (Some(a1), None) => f1
      case (None, Some(a2)) => f2
      case (None, None) => None
    }

    def zero: Option[A] = None
  }
```

Type class instances for 'transformers', such as `OptionT`, present a more subtle challenge. `OptionT[F, A]`
is a wrapper for a value of type `F[Option[A]]`. It allows us to write:

```scala
val ot = OptionT(List(Some(1), None))
ot.map((a: Int) => a * 2) // OptionT(List(Some(2), None))
```

The method `OptionT#map` requires an implicit parameter of type `Functor[F]`, whereas `OptionT#flatMap`
requires one of type `Monad[F]`. The capabilities of `OptionT` increase with those of `F`. We need to encode
this into the type class instances for `[a]OptionT[F[A]]`.

This is done with a hierarchy of [type class implementation traits](https://github.com/scalaz/scalaz/blob/scalaz-seven/core/src/main/scala/scalaz/OptionT.scala#L59)
and a corresponding set of [prioritized implicit methods](https://github.com/scalaz/scalaz/blob/scalaz-seven/core/src/main/scala/scalaz/OptionT.scala#L23).

In case of ambiguous implicits, Scala will favour one defined in a sub-class of the other. This is to avoid ambiguity
when in cases like the following:

```scala
type OptionTList[A] = OptionT[List[A]]
implicitly[Functor[OptionTList]]

// Candidates:
// 1. OptionT.OptionTFunctor[List](implicitly[Functor[List]])
// 2. OptionT.OptionTMonad[List](implicitly[Functor[List]])
// #2 is defined in a subclass of of the enclosing class of #1, so is preferred.
```

### Transformers and Identity

A stronger emphasis has been placed on transformer data structures (aka Monad Transformers). For example `State` is now
a type alias for `StateT[Id, A, B]`.

`Id` is defined in the `scalaz` package object as:

```scala
type Id[A] = A
```

## Contributing

[Documentation for contributors](https://github.com/scalaz/scalaz/blob/scalaz-seven/doc/DeveloperGuide.md)
