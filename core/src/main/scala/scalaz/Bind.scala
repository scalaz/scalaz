package scalaz

trait Bind[F[_]] {
  def bind[A, B](f: A => F[B]): F[A] => F[B]

  def deriving[G[_]](implicit n: ^**^[G, F]): Bind[G] =
    new Bind[G] {
      def bind[A, B](f: A => G[B]) =
        k => n.pack(Bind.this.bind((a: A) => n.unpack(f(a)))(n unpack k))
    }
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
