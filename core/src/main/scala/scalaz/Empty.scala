package scalaz

trait Empty[F[_]] {
  def empty[A]: F[A]
}

object Empty extends Emptys

trait Emptys {
  implicit def OptionEmpty: Empty[Option] = new Empty[Option] {
    def empty[A] = None
  }

  implicit def ListEmpty: Empty[List] = new Empty[List] {
    def empty[A] = Nil
  }

  implicit def StreamEmpty: Empty[Stream] = new Empty[Stream] {
    def empty[A] = Stream.empty
  }
}
