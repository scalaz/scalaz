package scalaz

import std.stream._
import EphemeralStream.EStream

trait Eq1[F[_]] {
  def eq1[A: Equal]: Equal[F[A]]
}

object Eq1 {

  def apply[F[_]](implicit F: Eq1[F]): Eq1[F] = F

  implicit val lazyOption: Eq1[LazyOption] =
    new Eq1[LazyOption] {
      def eq1[A: Equal] = Equal[LazyOption[A]]
    }

  implicit val stream: Eq1[Stream] =
    new Eq1[Stream] {
      def eq1[A: Equal] = Equal[Stream[A]]
    }

  implicit val estream: Eq1[EStream] = 
    new Eq1[EStream] {
      def eq1[A: Equal] = Equal[EStream[A]]
    }

}
