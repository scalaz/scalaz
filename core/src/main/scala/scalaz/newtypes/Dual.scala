package scalaz
package newtypes

sealed trait Dual[A] {
  val value: A
}

object Dual extends Duals

trait Duals {
  implicit def DualUnpack[A]: ^*^[Dual[A], A] =
    ^*^.^*^(_.value, b => new Dual[A] {
      val value = b
    })

  implicit def DualZero[A: Zero]: Zero[Dual[A]] =
    Zero.zero(implicitly[^*^[Dual[A], A]].pack(implicitly[Zero[A]].zero))

  implicit def DualSemigroup[A: Semigroup]: Semigroup[Dual[A]] = new Semigroup[Dual[A]] {
    def append(a1: Dual[A], a2: => Dual[A]) =
      implicitly[^*^[Dual[A], A]].pack(implicitly[Semigroup[A]].append(a2.value, a1.value))
  }

  implicit def DualMonoid[A: Monoid]: Monoid[Dual[A]] = {
    implicit val z = implicitly[Monoid[A]].zero
    implicit val s = implicitly[Monoid[A]].semigroup
    Monoid.monoid[Dual[A]]
  }
}