package scalaz


trait FoldMap[F[_]] {
  def foldMap[A, M: Monoid](f: A => M): F[A] => M

  def foldMapU[A, M: Monoid](f: A => M, a: F[A]): M = {
    val k = foldMap(f)
    k(a)
  }

  def fold[M: Monoid]: F[M] => M =
    foldMap(m => m)

  def foldU[M: Monoid](a: F[M]): M = {
    val k = fold
    k(a)
  }

}

object FoldMap extends FoldMaps

trait FoldMaps {
  implicit val ListFoldMap: FoldMap[List] = new FoldMap[List] {
    def foldMap[A, M: Monoid](f: A => M) =
      _.foldRight(implicitly[Monoid[M]].z) {
        case (a, b) => implicitly[Monoid[M]].append(f(a), b)
      }
  }
}