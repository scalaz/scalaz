package scalaz
package tc

import scala.Function1

import scala.language.experimental.macros
import scalaz.zio.Schedule

@meta.minimal("first", "second")
trait StrongClass[P[_, _]] extends ProfunctorClass[P] {
  def first[A, B, C](pab: P[A, B]): P[(A, C), (B, C)] =
    dimap[(C, A), (C, B), (A, C), (B, C)](second(pab))(_.swap)(_.swap)
  def second[A, B, C](pab: P[A, B]): P[(C, A), (C, B)] =
    dimap[(A, C), (B, C), (C, A), (C, B)](first(pab))(_.swap)(_.swap)
}

object StrongClass {
  implicit val functionStrong: Strong[? => ?] =
    instanceOf(new StrongClass[Function1] {
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
    })

  implicit def scheduleStrong: Strong[Schedule[?, ?]] =
    instanceOf(new StrongClass[Schedule[?, ?]] with ProfunctorClass[Schedule[?, ?]] {
      override def first[A, B, C](pab: Schedule[A, B]): Schedule[(A, C), (B, C)] =
        Schedule.apply[pab.State, (A, C), (B, C)](pab.initial,
                                                  (tup, s) =>
                                                    pab.update(tup._1, s) map {
                                                      _.rightMap(b => (b, tup._2))
                                                  })

      override def second[A, B, C](pab: Schedule[A, B]): Schedule[(C, A), (C, B)] =
        Schedule.apply[pab.State, (C, A), (C, B)](pab.initial,
                                                  (tup, state) =>
                                                    pab.update(tup._2, state) map {
                                                      _.rightMap(b => (tup._1, b))
                                                  })

      override def dimap[A, B, C, D](fab: Schedule[A, B])(ca: C => A)(bd: B => D): Schedule[C, D] =
        fab dimap (ca, bd)
    })
}

trait StrongSyntax {
  implicit final class ToStrongOps[F[_, _], A, B](self: F[A, B]) {
    def first[C](implicit ev: Strong[F]): F[(A, C), (B, C)] = macro ops.Ops.i_0
    def second[C](implicit ev: Strong[F]): F[(C, A), (C, B)] = macro ops.Ops.i_0
  }
}
