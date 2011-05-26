package scalaz

trait Each[F[_]] {
  def each[A](f: A => Unit): F[A] => Unit

  def deriving[G[_]](implicit n: ^**^[G, F]): Each[G] =
    new Each[G] {
      def each[A](f: A => Unit) =
        k => Each.this.each(f)(n.unpack(k))
    }
}

object Each extends Eachs

trait Eachs {
  implicit def OptionEach: Each[Option] = new Each[Option] {
    def each[A](f: A => Unit) = _ foreach f
  }

  implicit def StreamEach: Each[Stream] = new Each[Stream] {
    def each[A](f: A => Unit) = _ foreach f
  }
}