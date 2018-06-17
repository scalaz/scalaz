// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package examples

import java.lang.String
import scala.{ Boolean, Int }

import scalaz._
import Scalaz._

// a simple adt with typeclass derivations
package adt {
  sealed trait Foo
  final case class Bar(s: String)          extends Foo
  final case class Faz(b: Boolean, i: Int) extends Foo
  final case object Baz extends Foo {
    implicit val equal: Equal[Baz.type] =
      Decidable[Equal].xderiving0(Baz)
    implicit val default: Default[Baz.type] =
      Derives[Default].xderiving0(Baz)
  }
  object Bar {
    implicit val equal: Equal[Bar] =
      Decidable[Equal].xderiving1((s: String) => Bar(s), _.s)
    implicit val default: Default[Bar] =
      Derives[Default].xderiving1((s: String) => Bar(s), _.s)
  }
  object Faz {
    implicit val equal: Equal[Faz] =
      Decidable[Equal]
        .xderiving2((b: Boolean, i: Int) => Faz(b, i), f => (f.b, f.i))
    implicit val default: Default[Faz] =
      Derives[Default]
        .xderiving2((b: Boolean, i: Int) => Faz(b, i), f => (f.b, f.i))
  }
  object Foo {
    private[this] val to: (Bar \/ (Baz.type \/ Faz)) => Foo = {
      case -\/(bar)      => bar: Foo
      case \/-(-\/(baz)) => baz: Foo
      case \/-(\/-(faz)) => faz: Foo
    }
    private[this] val from: Foo => (Bar \/ (Baz.type \/ Faz)) = {
      case bar: Bar      => -\/(bar)
      case baz: Baz.type => \/-(-\/(baz))
      case faz: Faz      => \/-(\/-(faz))
    }

    implicit val equal: Equal[Foo] =
      Decidable[Equal].xcoderiving3(to, from)
    implicit val default: Default[Foo] =
      Derives[Default].xcoderiving3(to, from)
  }
}

// more complex recursive type example
package recadt {
  sealed trait ATree
  final case class Leaf(value: String)               extends ATree
  final case class Branch(left: ATree, right: ATree) extends ATree

  object Leaf {
    implicit val equal: Equal[Leaf] = {
      Decidable[Equal].xderiving1((v: String) => Leaf(v), _.value)
    }
    implicit val default: Default[Leaf] =
      Derives[Default].xderiving1((v: String) => Leaf(v), _.value)
  }
  object Branch {
    implicit val equal: Equal[Branch] =
      Decidable[Equal].xproduct2(
        implicitly[Equal[ATree]],
        implicitly[Equal[ATree]]
      )(
        (left: ATree, right: ATree) => Branch(left, right),
        b => (b.left, b.right)
      )
    implicit val default: Default[Branch] =
      Derives[Default].xproduct2(
        implicitly[Default[ATree]],
        implicitly[Default[ATree]]
      )(
        (left: ATree, right: ATree) => Branch(left, right),
        b => (b.left, b.right)
      )
  }
  object ATree {
    private[this] val to: Leaf \/ Branch => ATree = {
      case -\/(leaf)   => leaf
      case \/-(branch) => branch
    }
    private[this] val from: ATree => Leaf \/ Branch = {
      case leaf @ Leaf(_)        => -\/(leaf)
      case branch @ Branch(_, _) => \/-(branch)
    }

    implicit val equal: Equal[ATree] =
      Decidable[Equal].xcoproduct2(
        implicitly[Equal[Leaf]],
        implicitly[Equal[Branch]]
      )(to, from)
    implicit val default: Default[ATree] =
      Derives[Default].xcoderiving2(to, from)
  }

}

// more complex recursive GADT type example
package recgadt {
  sealed trait GTree[A]
  final case class GLeaf[A](value: A)                          extends GTree[A]
  final case class GBranch[A](left: GTree[A], right: GTree[A]) extends GTree[A]

  object GLeaf {
    implicit def equal[A: Equal]: Equal[GLeaf[A]] =
      Decidable[Equal].xderiving1((v: A) => GLeaf(v), _.value)
    implicit def default[A: Default]: Default[GLeaf[A]] =
      Derives[Default].xderiving1((v: A) => GLeaf(v), _.value)
  }
  object GBranch {
    implicit def equal[A: Equal]: Equal[GBranch[A]] =
      Decidable[Equal].xproduct2(
        implicitly[Equal[GTree[A]]],
        implicitly[Equal[GTree[A]]]
      )(
        (left: GTree[A], right: GTree[A]) => GBranch(left, right),
        b => (b.left, b.right)
      )
    implicit def default[A: Default]: Default[GBranch[A]] =
      Derives[Default].xproduct2(
        implicitly[Default[GTree[A]]],
        implicitly[Default[GTree[A]]]
      )(
        (left: GTree[A], right: GTree[A]) => GBranch(left, right),
        b => (b.left, b.right)
      )
  }
  object GTree {
    private[this] def to[A]: GLeaf[A] \/ GBranch[A] => GTree[A] = {
      case -\/(leaf)   => leaf
      case \/-(branch) => branch
    }
    private[this] def from[A]: GTree[A] => GLeaf[A] \/ GBranch[A] = {
      case leaf @ GLeaf(_)        => -\/(leaf)
      case branch @ GBranch(_, _) => \/-(branch)
    }

    implicit def equal[A: Equal]: Equal[GTree[A]] =
      Decidable[Equal].xcoproduct2(
        implicitly[Equal[GLeaf[A]]],
        implicitly[Equal[GBranch[A]]]
      )(to, from)
    implicit def default[A: Default]: Default[GTree[A]] =
      Derives[Default].xcoproduct2(
        implicitly[Default[GLeaf[A]]],
        implicitly[Default[GBranch[A]]]
      )(to, from)
  }

}
