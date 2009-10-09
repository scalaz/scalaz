package scalaz

/**
 * Gets the first element of an environment.
 */

trait First[F[_]] {
  def first[A](a: F[A]): Option[A]
}

object First {
  implicit val ListFirst = new First[List] {
    override def first[A](as: List[A]): Option[A] = as match {
      case Nil => None
      case a :: _ => Some(a)
    }
  }
}
