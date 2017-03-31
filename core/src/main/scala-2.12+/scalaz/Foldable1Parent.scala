package scalaz

////
import syntax.foldable1.ToFoldable1Ops
////
trait Foldable1Parent[F[_]] { self: Foldable1[F] =>
  ////

  def msuml1[G[_]: Plus, A](fa: F[G[A]]): G[A] =
    ToFoldable1Ops(fa)(self).msuml1

  def toNel[A](fa: F[A]): NonEmptyList[A] =
    foldMapRight1(fa)(NonEmptyList.nel(_, Nil))(_ <:: _)

  ////
}
