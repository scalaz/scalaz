package scalaz

trait Bind[F[_]] {
  def bind[A, B](f: A => F[B]): F[A] => F[B]
}

object Bind extends Binds

trait Binds {
  implicit val OptionBind: Bind[Option] = new Bind[Option] {
    def bind[A, B](f: A => Option[B]) =
      _ flatMap f
  }

  implicit val ListBind: Bind[List] = new Bind[List] {
    def bind[A, B](f: A => List[B]) =
      _ flatMap f
  }

  implicit val StreamBind: Bind[Stream] = new Bind[Stream] {
    def bind[A, B](f: A => Stream[B]) =
      _ flatMap f
  }
}
