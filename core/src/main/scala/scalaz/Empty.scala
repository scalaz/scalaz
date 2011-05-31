package scalaz

import scalaz.Scalaz._

trait Empty[F[_]] {
  def empty[A]: F[A]

  def deriving[G[_]](implicit n: ^**^[G, F]): Empty[G] =
    new Empty[G] {
      def empty[A] =
        n.pack(Empty.this.empty[A])
    }
}

object Empty extends Emptys

trait Emptys extends EmptysLow {
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

trait EmptysLow {

  import collection.TraversableLike

  implicit def TraversableEmpty[CC[X] <: TraversableLike[X, CC[X]] : CanBuildAnySelf]: Empty[CC] = new Empty[CC] {
    def empty[A] = {
      val builder = implicitly[CanBuildAnySelf[CC]].apply[⊥, A]
      builder.result
    }
  }
}