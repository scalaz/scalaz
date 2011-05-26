package scalaz
package data

sealed trait Endo[A] {
  def apply(a: => A): A

  import Endo._

  def fix: A = apply(fix)

  def compose(e: Endo[A]): Endo[A] =
    endo(a => apply(e(a)))
}

object Endo extends Endos {
  def apply[A](f: (=> A) => A): Endo[A] =
    endo(f)
}

trait Endos {
  def endo[A](f: (=> A) => A): Endo[A] = new Endo[A] {
    def apply(a: => A): A =
      f(a)
  }

  def constEndo[A](a: => A): Endo[A] = endo(_ => a)

  def idEndo[A]: Endo[A] = endo(z => z)

  implicit def EndoTo[A](e: Endo[A]): (=> A) => A =
    a => e.apply(a)

  implicit def EndoNewtype[A]: ^*^[Endo[A], (=> A) => A] =
    ^*^.^*^(e => e(_), b => new Endo[A] {
      def apply(a: => A) = b(a)
    })

  implicit def EndoZero[A]: Zero[Endo[A]] =
    Zero.zero(endo(x => x))

  implicit def EndoSemigroup[A]: Semigroup[Endo[A]] = new Semigroup[Endo[A]] {
    def append(a1: Endo[A], a2: => Endo[A]) =
      a1 compose a2
  }

  implicit def EndoMonoid[A]: Monoid[Endo[A]] =
    Monoid.monoid
}
