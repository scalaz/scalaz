package scalaz
package newtypes

import scalaz.{Newtype, Pack, Unpack}

sealed trait Dual[A] extends Pimp[A]

object Dual extends Duals

trait Duals {
  implicit def DualUnpack[A]: Unpack[Dual[A], A] = new Unpack[Dual[A], A] {
    val unpack = (_: Dual[A]).value
  }

  implicit def DualPack[A]: Pack[Dual[A], A] = new Pack[Dual[A], A] {
    val pack = (b: A) => new Dual[A] {
      val value = b
    }
  }

  implicit def DualNewtype[A]: Newtype[Dual[A], A] =
    Newtype.newtype

  implicit def DualZero[A: Zero]: Zero[Dual[A]] =
    Zero.zero(implicitly[Pack[Dual[A], A]].pack(implicitly[Zero[A]].zero))

  implicit def DualSemigroup[A: Semigroup]: Semigroup[Dual[A]] = new Semigroup[Dual[A]] {
    def append(a1: Dual[A], a2: => Dual[A]) =
      implicitly[Pack[Dual[A], A]].pack(implicitly[Semigroup[A]].append(a2.value, a1.value))
  }

  implicit def DualMonoid[A: Monoid]: Monoid[Dual[A]] = {
    implicit val z = implicitly[Monoid[A]].zero
    implicit val s = implicitly[Monoid[A]].semigroup
    Monoid.monoid[Dual[A]]
  }
}