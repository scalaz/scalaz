package scalaz

import std.stream._

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

}
