package scalaz

trait Foldable[F[_]] {
  val foldr: Foldr[F]
  val foldl: Foldl[F]

  import Foldable._

  def foldRight[A, B] =
    foldr.foldr[A, B]

  def foldLeft[A, B] =
    foldl.foldl[A, B]

  def deriving[G[_]](implicit n: ^**^[G, F]): Foldable[G] = {
    implicit val r: Foldr[G] = foldr.deriving[G]
    implicit val l: Foldl[G] = foldl.deriving[G]
    foldable[G]
  }
}

object Foldable extends Foldables

trait Foldables extends FoldablesLow {
  def foldable[F[_]](implicit r: Foldr[F], l: Foldl[F]): Foldable[F] = new Foldable[F] {
    val foldr = r
    val foldl = l
  }

  implicit val ListFoldable: Foldable[List] =
    foldable[List]

  implicit val StreamFoldable: Foldable[Stream] =
    foldable[Stream]

  implicit val OptionFoldable: Foldable[Option] =
    foldable[Option]
}

trait FoldablesLow {
  implicit def TraversableFoldable[CC[X] <: Traversable[X]]: Foldable[CC] =
    Foldable.foldable
}
