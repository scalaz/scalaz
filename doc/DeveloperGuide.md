# Developer Guide

This document describes the organization and coding conventions used in the library. It is intended for contributors,
but may also be of interest to users.

## Module Structure

`scalaz-core` should be kept lean and mean: type classes and instances, important data structures, and functions
related to these.

`scala.{iteratee, effect, concurrent}` have each been moved a dedicated sub-project. Consider the same approach
when adding large new features.

## Type Classes

### Inheritance

Type class extension is encoded with inheritance. For example, `Monoid` extends `Semigroup`.

### Hierarchy

The type class hierarchy is configured in the build (`GenTypeClass`). The SBT command `gen-type-classes`
will recreate all type classes, preserving chunks of code delimited by pairs of `////` comments.

Do not add code, comments, or imports outside of these delimiters.

### Methods

The abstract methods of a type class should be listed first. Derived methods follow under a comment `// derived methods`.

Method should not have symbolic identifiers. (Symbolic aliases are allowed under `scalaz.syntax._`.)

Methods parameters should be organized to support type inference when using the type class directly.

    def map[A, B](fa: F[A])(f: A => B): F[B]  // good
    def map[A, B](fa: F[A], f: A => B): F[B]  // bad

Use currying sparingly. Less indirection helps performance. Curried versions of methods may be offered as alternatives.

    def fold[A, B](fa: F[A])(f: (A, B) => B): F[B] // good
    def fold[A, B](fa: F[A])(f: A => B => B): F[B] // bad
    def fold[A, B](fa: F[A]): A => B => B => F[B] // bad
    final def foldCurried[A, B](f: F[A]): (A => B => B) => F[B] = f => fold(fa)((a, b) => f(a)(b)) // okay

Add general purpose derived methods directly to the type class. These may be left unfinalized, to allow a
child type class or a type class instance to override them for efficiency.

## Data Structures

Minimize the number of top level declarations associated with a data structure. In particular, use the
companion object to scope the elements of the ADT. This avoids clutter in Scaladoc and namespace pollution
for users who `import scalaz._`.

Offer direct access to methods like `map`, `flatMap`, and `foldMap` directly as class methods in the data structure.
Type class implementations should delegate to these. This makes these methods more discoverable via Scaladoc and
autocompletion, and avoids unneeded indirection in cases when abstraction is not required.

Define type class instances in the same files as the data structure. Type class instance is described in more detail separately.

    sealed abstract trait MyDataStructure[A] {
       final def map[B](f: A => B): F[B] = ...
       final def flatMap[B](f: A => F[B]): F[B] = ...
    }

    object MyDataStructure extends MyDataStructureFunctions with MyDataStructureInstances {
       // Define type aliases in the companion directly; avoids fights with the compiler later.
       type MDS[A] = MyDataStructure[A]

       // apply method not suitable for mixing, define directly in the companion.
       def apply[A](a: A): MyDataStructure[A] = ...
    }

    // described separately.
    trait MyDataStructureInstances {
       import MyDataStructure._
       type MDS[X] = MyDataStructure

       implicit object mdsMonad extends Monad[MDS] {
          def point[A](a: => A) = MDS(a)

          def bind[A, B](fa: MDS[A])(f: A => MDS[B]) = fa flatMap f // delegate

          // override to use directly call map
          override def map[A, B](fa: MDS[A])(f: A => B) = fa map f // delegate
       }
    }

    // Design to allow the user to mix these functions into an object
    trait MyDataStructureFunctions {
        final def emptyMds[A]: MyDataStructure[A] = ...
    }

## Type Class Instances

### General

Type class instances are packaged in a trait `ClassifiedTypeInstances`.

The implicit members within the `Instances` class should be named:

`classifiedTypeInstance` // the primary instance, e.g. `implicit object optionInstance extends MonadPlus[Option] with Traverse[Option]`.
`classifiedTypeTypeClassName` // additional instances, e.g. `implicit def optionSemigroup[A]`

### Scala and Java standard library

Instances are organized under `scalaz.std` according to the package of the classified type. The package prefix `scala`
is omitted. Where a type is aliased under the package `scala`, the shorter path to the alias determines the location
of the type class.

`scala.Option` => `scalaz.std.option._`
`scala.List` => `scalaz.std.list._`
`scala.math.BigInt` => `scalaz.std.math.bigInt`
`java.math.BigInteger` => `scalaz.std.java.math.bigInteger`

In the file `ClassifiedType.scala`, define the trait `ClassifiedTypeInstances`, and mix it into a) an object `classifiedType`,
and b) `scalaz.std.AllInstances`.

In some cases, a group of related type class instances may be defined in a single file, such as for `AnyVals` or `TupleN`.

### Scalaz Data Structures

Define the `MyDatastructureInstances` and mix this into `object MyDataStructure`.

### Type Class Dependencies

A type class instances for `F[X]` may depend on an instance for `X`. This can be extended to dependencies on
multiple instances.

For example:

`(Semigroup[A], Semigroup[B]) => Semigroup[(A, B)]`
`(Monoid[A], Monoid[B]) => Monoid[(A, B)]`

This demands careful organization of these implicits to ensure that `implicitly[Semigroup[(Int, Int)]` resolves
without ambiguity (both of these functions could provide that).

Here's how to organize the type class instances and implicits for `Tuple2`. See the comments inline.

    trait TupleInstances0 {
      // defined in a supertype of TupleInstances as a tie-breaker in case of ambiguity.
      // pass the type class instances into the type class implementation trait as members.
      // We need to choose different names for the parameters and the members to avoid shadowing.
      // Usually we suffix the parameter name with '0', in this case '_' is used for better readability.
      implicit def tuple2Semigroup[A1, A2](implicit A1_ : Semigroup[A1], A2_ : Semigroup[A2]): Semigroup[(A1, A2)] = new Tuple2Semigroup[A1, A2] {
        // WARNING: It's really easy to write `def A1 = A1` here!!
        implicit def A1 = A1_
        implicit def A2 = A2_
      }
    }

    trait TupleInstances extends TupleInstances0 {
      // In a subtype of tuple2Monoid. The order doesn't matter, but by convention we follow the subtyping
      // relationship of the type classes.
      implicit def tuple2Monoid[A1, A2](implicit A1: Monoid[A1], A2: Monoid[A2]): Monoid[(A1, A2)] = new Tuple2Monoid[A1, A2] {
        implicit def A1: Monoid[A1] = A1_
        implicit def A2: Monoid[A2] = A2_
      }

      // It would be okay to have `tuple2Bifunctor` in this trait, as there is no chance of ambiguity with
      // tuple2Monoid in an implicit search.
    }

    object tuple extends TupleInstances

    //
    // Type class implementation traits
    //
    private[scalaz] trait Tuple2Semigroup[A1, A2] extends Semigroup[(A1, A2)] {
      implicit def A1 : Semigroup[A1]  // Requirements about the type
      implicit def A2 : Semigroup[A2]

      def append(f1: (A1, A2), f2: => (A1, A2)): (A1, A2) = (
        A1.append(f1._1, f2._1),
        A2.append(f1._2, f2._2)
      )
    }

    // Inherits from `Tuple2Semigroup` to get `append`. Put `Tuple2Inheritances` at the end
    // of the extends list to use its definitions preferentially. This becomes important
    // when using `Monad` and `Traverse`.
    //
    // In some cases, you need to add `override` to the methods in these traits to allow this
    // ordering of the extends list. For example, see `OptionTFunctor#map`
    //
    private[scalaz] trait Tuple2Monoid[A1, A2] extends Monoid[(A1, A2)] with Tuple2Semigroup[A1, A2] {
      implicit def A1 : Monoid[A1] // refining the implicit requirement
      implicit def A2 : Monoid[A2]

      def zero: (A1, A2) = (A1.zero, A2.zero)
    }

This pattern is applied for type class instances for the transformers (e.g `OptionT`, `EitherT`),
for instances over compositions and products, and for instances derived via an isomorphism.

## Syntax

The methods of a type class that should be copied to the corresponding `TypeClassOps` object.

For example:

    trait FunctorOps[F[_],A] extends Ops[F[A]] {
      implicit def F: Functor[F]

      final def map[B](f: A => B): F[B] = F.map(self)(f)

      // such esoterica should be restricted to these wrappers.
      final def ∘[B](f: A => B): F[B] = F.map(self)(f)
    }

Do not add non-trivial methods implementations to these wrappers. Instead, define the method on the type class
and delegate. A user should be able to opt-out of the syntax package without losing access to functionality.

The type class generation machinery maintains these files, again, just fill in the gaps. The generated implicit
conversions use the `Unapply` and `Unapply2` types to maximize type inference, see this in action in
`scalaz.example.UnapplyInference`. The same technique may be used in the methods in the syntax wrapper,
see `TraverseOps#sequence`.

Standard library types, and Scalaz data structures may also have a syntax defined. These files and implicit conversions
are created by hand. For example, see `scalaz.syntax.TreeOps` and `scalaz.syntax.std.OptionOps`. Remember to add these to
either `ToDataOps` or `ToAllStdOps`, and to update `Syntaxes`.

All the syntactic implicit conversions are eventually mixed into the object Scalaz. However, it is now possible, and
recommended, to be more fine grained with imports.

    import scalaz._, syntax.applicative._, std.option._  // fine grained imports: preferred
    import scalaz._, Scalaz._                            // über import: convenient, but clutters namespace and slows compiler/IDE

Because all the conversions are packaged in traits, it is possible for users to create objects that aggregate a smaller
set of the imports.

## General

### Type Parameters

In the absence of a context specific name for the type parameters, use these:

 * Use `A`, `B`, `C`, `X` for kind `*`
 * Use `F`, `G` for kind `* -> *`.

Declare type parameters requiring type constructors first in type parameter declarations. This is an arbitrary choice,
but the consistency is worth it.

    class Foo[F[_], A, B] // good
    class Foo[A, F[_], B] // bad

Define type lambdas with either lower case or greek letters as the type parameters. This helps to distinguish them
from the applied types.

    ({type λ[α]=F[X, α]})#λ
    ({type λ[α, β]=F[X, α, β]})#λ
    ({type l[a]=F[X, a]})#l
    ({type l[a, b]=F[X, a, b]})#l

### Parameters

Similarly, use these as defaults for parameters:

 * Use `a` for a parameter of type `A`
 * Use `fa` for a parameter of type `F[A]`
 * Use `F` for an implicit parameter of type `TypeClass[F]`.

### Type annotations

 * Annotate the return type of all public methods
   * Exception: overriding methods, including in type classes
   * Exception: implicits defining type class instances. (TODO Are there still any corner cases in scalac where this will bite us?)

### Using type classes

 * Do not use `scalaz.syntax._` to implement `scalaz._`. Instead use the type classes directly.
   Every type class companion object has an `apply` method to obtain an instance: use `Monad[M].bind` instead of
   `implicitly[Monad[M]].bind`

### Build Errors

 * Use of package objects has led to intermittent failures in incremental compilation, such as
   "package scalaz.syntax refers to nonexisting symbol." or a NPE. Use clean build as a workaround.

### OSGi Support

All JARs contain OSGi metadata and are usable in an OSGi container without modification.  The metadata is generated automatically
by [sbt-osgi](https://github.com/sbt/sbt-osgi), which delegates the heavy lifting to [bnd](http://www.aqute.biz/Code/Bnd).

All packages of each JAR are exported with the package version set to the version of Scalaz.  All bundles import all used packages, with
some exceptions for optional imports (unsatisfied optional imports will result in ClassNotFoundExceptions at runtime if dependent code
paths are exercised).

To maintain the OSGi metadata, the OsgiKeys.* settings should be updated appropriately as new packages are created or existing packages
are removed or renamed.  Further, care should be taken to ensure each package is wholly contained by a single JAR.

If a new project is added, the exported packages must be defined by declaring an OsgiKeys.exportedPackages setting, typically via
the osgiExport method:

    osgiExport("scalaz.newproject")


## How can I help?

 * Port some examples, or create new ones, to get a feel for the new organization.
 * Port/Write test cases
 * Port a missing data structure
 * Add type class instances (most `Show`, `Equal`, `Ordering` are missing).
 * Documentation
    * Class level documentation for each type class.
    * Brief method documentation welcome
    * `core/show-doc` in SBT will build and pop up the scaladoc.
 * Review code base for consistency problems
 * Review type class hierarchy
