package scalaz
package tc

import Predef._

import scala.List

trait MonoidClass[A] extends SemigroupClass[A] {
  def mempty: A
}

object MonoidClass {
  implicit val stringMonoid: Monoid[String] = instanceOf(new MonoidClass[String] {
    def mappend(a1: String, a2: => String) = a1 + a2
    def mempty                             = ""
  })

  implicit val unitMonoid: Monoid[Unit] = instanceOf(new MonoidClass[Unit] {
    def mappend(a1: Unit, a2: => Unit) = ()
    def mempty                         = ()
  })

  implicit def listMonoid[A]: Monoid[List[A]] =
    instanceOf(new MonoidClass[List[A]] {
      def mappend(a1: List[A], a2: => List[A]): List[A] = a1 ++ a2
      def mempty                                        = List.empty
    })

}
