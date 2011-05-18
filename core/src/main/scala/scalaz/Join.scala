package scalaz

trait Join[F[_]] {
  def join[A]: F[F[A]] => F[A]
}

object Join extends Joins

trait Joins {
  implicit val OptionJoin: Join[Option] = new Join[Option] {
    def join[A] =
      _ flatMap identity
  }

  implicit val ListJoin: Join[List] = new Join[List] {
    def join[A] =
      _ flatMap identity
  }

  implicit val StreamJoin: Join[Stream] = new Join[Stream] {
    def join[A] =
      _ flatMap identity
  }
}
