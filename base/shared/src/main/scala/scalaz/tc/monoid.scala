package scalaz
package tc

import Predef._

import java.lang.Throwable
import scala.{List, Nothing}

import zio.{ Fiber, IO }

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

  implicit def fiberMonoid[E, A](implicit A: Monoid[A]): Monoid[Fiber[E, A]] =
    instanceOf(new MonoidClass[Fiber[E, A]] {
      def mappend(a1: Fiber[E, A], a2: => Fiber[E, A]) = a1.zipWith(a2)(A.mappend(_, _))
      def mempty = new Fiber[E, A] {
        def join: IO[E, A]                             = IO.now(A.mempty)
        def interrupt(t: Throwable): IO[Nothing, Unit] = IO.now(())
      }
    })
}
