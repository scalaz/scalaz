package scalaz

trait Each[F[_]] {
  def each[A](f: A => Unit): F[A] => Unit
}

object Each extends Eachs

trait Eachs {
  implicit def OptionEachEach: Each[Option] = new Each[Option] {
    def each[A](f: A => Unit) = _ foreach f
  }
}