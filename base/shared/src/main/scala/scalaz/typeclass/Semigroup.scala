package scalaz
package typeclass

sealed trait Semigroup[A] extends Semigroup.Class[A]

object Semigroup {

  trait Class[A] {
    def append(a1: A, a2: => A): A

    def semigroup: Semigroup[A]
  }

  trait Template[A] extends Semigroup[A] {
    final override def semigroup = this
  }

  def apply[T](implicit T: Semigroup[T]): Semigroup[T] = T
}