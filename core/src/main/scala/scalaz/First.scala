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

  implicit val NonEmptyListFirst = new First[NonEmptyList] {
    override def first[A](as: NonEmptyList[A]): Option[A] = Some(as.head)
  }

  implicit val StreamFirst = new First[Stream] {
    override def first[A](as: Stream[A]): Option[A] = {
      if (as.isEmpty) None else Some(as.head)
    }
  }

  implicit val OptionFirst = new First[Option] {
    override def first[A](as: Option[A]): Option[A] = as
  }

}
