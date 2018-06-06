package scalaz
package ct

import scala.language.experimental.macros

@meta.minimal("first", "second")
trait StrongClass[P[_, _]] extends ProfunctorClass[P] {
  def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)] =
    dimap[(C, A), (C, B), (A, C), (B, C)](second(pab))(_.swap)(_.swap)

  def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)] =
    dimap[(A, C), (B, C), (C, A), (C, B)](first(pab))(_.swap)(_.swap)
}

trait StrongInstances { instances =>

  implicit val functionStrong: Strong[? => ?] = instanceOf(
    new StrongClass[? => ?] {

      override def lmap[A, B, C](fab: A => B)(ca: C => A): C => B =
        fab compose ca

      override def rmap[A, B, C](fab: A => B)(bc: B => C): A => C =
        fab andThen bc

      override def first[A, B, C](pab: A => B): ((A, C)) => (B, C) = {
        case (a, c) => (pab(a), c)
      }

      override def second[A, B, C](pab: A => B): ((C, A)) => (C, B) = {
        case (c, a) => (c, pab(a))
      }
    }
  )
}

trait StrongSyntax {
  implicit final class ToStrongOps[F[_, _], A, B](self: F[A, B]) {
    def first[C](implicit ev: Strong[F]): F[(A, C), (B, C)] = macro meta.Ops.i_0
    def second[C](implicit ev: Strong[F]): F[(C, A), (C, B)] = macro meta.Ops.i_0
  }
}
