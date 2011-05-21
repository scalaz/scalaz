package scalaz

trait Length[F[_]] {
  def len[A](a: F[A]): Int
}

object Length extends Lengths

trait Lengths {
  implicit def OptionLength: Length[Option] = new Length[Option] {
    def len[A](a: Option[A]) = a match {
      case Some(_) => 1
      case None => 0
    }
  }

  implicit def ArrayLength: Length[Array] = new Length[Array] {
    def len[A](a: Array[A]) =
      a.length
  }

  implicit def ListLength: Length[List] = new Length[List] {
    def len[A](a: List[A]) =
      a.length
  }
}
