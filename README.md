## Scalaz Seven

This is an experimental fork of [Scalaz](https://github.com/scalaz/scalaz) for prototyping
changes for [Scalaz 7](http://code.google.com/p/scalaz/wiki/Scalaz7).

This is only a candidate design for Scalaz 7. An alternative design, which eschews type class inheritance,
is available in the [scalaz7](https://github.com/scalaz/scalaz/tree/scalaz7) branch in the main repository.

### Modularity

Scalaz has grown to contain a wide range of useful type classes and data structures. This presents a challenge
to the beginner trying to learn the library -- where to start, what to ignore? To help alleviate this, the
library has been modularised further.

* **scalaz-core**: Type class hierarchy, data structures, type class instances for the Scala and Java standard libraries,
                 implicit conversions / syntax to access these.
* **scalaz-effect**: Data structures to represent and compose IO effects in the type system.
* **scalaz-concurrent**: Actor and Promise implementation
* **scalaz-iteratee**: Iteratee implementation

### Type Class Hierarchy

* Type classes form an inheritance hierarchy, as in Scalaz 6. This is convenient both at the call site and at the
  type class instance definition. Considering for now the call site, it ensures the following code is valid:

```
def bar[M: Functor] = ()

def foo[M: Monad] = bar
```

* From a high level, the hierarchy itself is largely the same as in Scalaz 6. However, there have been a few
 adjustments, some signatures have been adjusted to support better standalone usage, so code depending on these will
 need to be re-worked.

### Standalone Type Class Usage

Type classes should be directly usable, without first needing to trigger implicit conversions. This might be
desirable to reduce the runtime or cognitive overhead of the pimped types, or to define your own pimped
types with a syntax of your choosing.

* The methods in type classes have been curried to maximize type inference.
* Derived methods, based on the abstract methods in a type class, are defined in the type class itself.
* Each type class companion object is fitted with a convenient `apply` method to obtain an instance of the type class.

```
// Equivalent to `implicitly[Monad[Option]]`
val O = Monad[Option]

// `bind` is defined with two parameter sections, so that the type of `x` is inferred as `Int`.
O.bind(Some(1))(x => Some(x * 2)

def plus(a: Int, b: Int) = a + b

// `Apply#lift2` is a function derived from `Apply#ap`.
val plusOpt = O.lift2(plus)
```

### Type Class Instance Definition

* 'Constructive' implicits, which create a type class instance automatically based on instances of
  all parent type classes, are removed. These led to subtle errors with ambiguous implicits, such as
  this problem with [FunctorBindApply](http://stackoverflow.com/questions/7447591/how-do-i-use-name-as-an-applicative/7448111#7448111)
* Type class instances are no longer declared in fragments in the companion objects of the type class. Instead, they
  are defined in the package `scalaz.std`, and must be imported. These instances are defined in traits which will be
  mixed together into an object for importing *en-masse*, if desired.
* A single implicit can define a number of type class instances for a type.
* A type class definition can override methods (including derived methods) for efficiency.

Here is an instance definition for `Option`. Notice that the method `map` has been overriden.

```
  implicit val option = new MonadPlus[Option] with Traverse[Option] {
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

```
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

* Syntax is segregated from rest of the library, in a sub-package `scalaz.syntax`.
* All Scalaz functionality is available *without* using the provided syntax, by directly calling methods
  on the type class or it's companion object.
* Syntax is available *a-la-carte*. You can import the syntax for working with a particular
  type classes where you need it. This avoids flooding the autocompletion in your IDE with
  every possible pimped method. In principle, this should also help compiler performance,
  by reducing the implicit search space.
* Syntax is layered in the same way as type classes. Importing the syntax for, say, `Applicative`
  will also provide the syntax for `Pointed` and `Functor`.
* Use of symbolic identifiers is largely restricted to the `syntax` package.

Syntax can be imported in two ways. Firstly, the syntax specialized for a particular instance
of a type class can be imported directly from the instance itself.

```scala
// import the type class instance
import scalaz.std.option.optionInstance

// import the implicit conversions to `MonadV[Option, A]`, `BindV[Option, A]`, ...
import optionInstance.monadSyntax._

val oi: Option[Option[Int]] = Some(Some(1))

// Expands to: `ToBindV(io).join`
oi.join
```

Alternatively, the syntax can be imported for a particular type class.

```scala
// import the type class instance
import scalaz.option.optionInstance

// import the implicit conversions to `MonadV[F, A]`, `BindV[F, A]`, ...
import scalaz.syntax.monad._

val oi: Option[Option[Int]] = Some(Some(1))

// Expands to: ToBindV(io).join
oi.join
```

For some degree of backwards compability with Scalaz 6, the über-import of `import scalaz.Scalaz._`
will import *all* implicit conversions that provide syntax (as well as type class instances and other
functions). However, it is recommended to review usage of this and replace with more focussed imports.


### Type Class Instance Dependencies

Type class instances may depend on other instances. In simple cases, this is as straightforward as adding an implicit
parameter (or, equivalently, a context bound), to the implicit method.

```
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

```
val ot = OptionT(List(Some(1), None))
ot.map((a: Int) => a * 2) // OptionT(List(Some(2), None))
```

The `OptionT#map` requires `Functor[F]`, whereas `OptionT#flatMap` requires `Monad[F]`. The capabilities of
`OptionT` increase with those of `F`. We need to encode this into the type class instances for `[a]OptionT[F[A]]`.

This is done with a hierarchy of [type class implementation traits](https://github.com/retronym/scalaz7-experimental/blob/master/core/src/main/scala/scalaz/OptionT.scala#L59)
and a corresponding set of [prioritized implicit methods](https://github.com/retronym/scalaz7-experimental/blob/master/core/src/main/scala/scalaz/OptionT.scala#L23).

In case of ambiguous implicits, Scala will favour one defined in a sub-class of the other. This is to avoid ambiguity
when in cases like the following:

```scala
type OptionTList[A] = OptionT[List[A]]
implicitly[Functor[OptionTList]]

// Candidates:
// 1. OptionT.OptionTFunctor[List](implicitly[Functor[List]])
// 2. OptionT.OptionTMonad[List](implicitly[Functor[List]])
// #2 is defined in a subclass, so is preferred (although, either would have sufficed).
```

### Transformers and Identity

A stronger emphasis has been placed on transformer data structures (aka Monad Transformers).

TODO: Describe relationship between `Id` / `Value` / `Name` / `Need`, show usage in a transformer.

### Deriving Type Class Instances through Isomorphisms

https://github.com/retronym/scalaz7-experimental/blob/master/core/src/main/scala/scalaz/Isomorphism.scala

### Deriving Type Class Instances through Composition / Product

https://github.com/retronym/scalaz7-experimental/blob/master/core/src/main/scala/scalaz/Composition.scala

https://github.com/retronym/scalaz7-experimental/blob/master/core/src/main/scala/scalaz/Product.scala

### Unboxed Tagged Types

https://github.com/retronym/scalaz7-experimental/blob/master/core/src/main/scala/scalaz/std/AnyVal.scala#L35
