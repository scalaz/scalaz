package scalaz
package tc

import Predef._

import zio.{ Fiber, IO }

import java.lang.Throwable
import scala.{ List, Nothing, Unit }

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
      def mempty: List[Nothing]                         = List.empty
    })

  implicit def fiberMonoid[E, A](implicit A: Monoid[A]): Monoid[Fiber[E, A]] =
    instanceOf(new MonoidClass[Fiber[E, A]] {
      def mappend(a1: Fiber[E, A], a2: => Fiber[E, A]): Fiber[E, A] = a1.zipWith(a2)(A.mappend(_, _))
      def mempty: Fiber[E, A] = new Fiber[E, A] {
        def join: IO[E, A]                                     = IO.now(A.mempty)
        def interrupt0(ts: List[Throwable]): IO[Nothing, Unit] = IO.unit
      }
    })

  implicit def ioMonoid[E, A](implicit SE: Semigroup[E], A: Monoid[A]): Monoid[IO[E, A]] =
    instanceOf(new MonoidClass[IO[E, A]] {
      def mappend(a1: IO[E, A], a2: => IO[E, A]): IO[E, A] =
        a1.redeem(e1 => a2.redeem(e2 => IO.fail(SE.mappend(e1, e2)), _ => IO.fail(e1)),
                  s1 => a2.redeem(e2 => IO.fail(e2), s2 => IO.point(A.mappend(s1, s2))))
      def mempty: IO[E, A] = IO.now(A.mempty)
    })
}
