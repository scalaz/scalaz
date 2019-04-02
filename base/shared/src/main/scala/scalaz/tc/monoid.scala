package scalaz
package tc

import Predef._

import zio.{ Fiber, Schedule }

import scala.{ List, Unit }

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
      def mempty: List[A]                               = List.empty
    })

  implicit def fiberMonoid[E, A](implicit A: Monoid[A]): Monoid[Fiber[E, A]] =
    instanceOf(new MonoidClass[Fiber[E, A]] {
      def mappend(a1: Fiber[E, A], a2: => Fiber[E, A]): Fiber[E, A] =
        a1.zipWith(a2)(A.mappend(_, _))
      def mempty: Fiber[E, A] = Fiber.point(A.mempty)
    })

  implicit def strategyMonoid[E, A](implicit A: Monoid[A]): Monoid[Schedule[E, A]] =
    instanceOf(new MonoidClass[Schedule[E, A]] {
      override def mempty: Schedule[E, A] = Schedule.point(A.mempty)

      override def mappend(a1: Schedule[E, A], a2: => Schedule[E, A]): Schedule[E, A] =
        (a1 && a2).map(tup => A.mappend(tup._1, tup._2))
    })
}
