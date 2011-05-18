package scalaz

trait Foldable[F[_]] {
  val foldr: Foldr[F]
  val foldl: Foldl[F]

  def foldRight[A, B] =
    foldr.foldr[A, B]

  def foldLeft[A, B] =
    foldl.foldl[A, B]
}

object Foldable extends Foldables

trait Foldables {
  def foldable[F[_]](implicit r: Foldr[F], l: Foldl[F]): Foldable[F] = new Foldable[F] {
    val foldr = r
    val foldl = l
  }

  implicit val ListFoldable: Foldable[List] =
    foldable[List]

  implicit val StreamFoldable: Foldable[Stream] =
    foldable[Stream]
}
