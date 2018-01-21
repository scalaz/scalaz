package scalaz
package typeclass

sealed trait Monoid[A] extends Monoid.Class[A]

object Monoid {

  trait Class[A] extends Semigroup.Class[A] {
    def empty: A

    def monoid: Monoid[A]
  }

  trait Template[A] extends Semigroup.Template[A] with Monoid[A]  {
    final override def monoid = this
  }

  def apply[T](implicit T: Monoid[T]): Monoid[T] = T

}