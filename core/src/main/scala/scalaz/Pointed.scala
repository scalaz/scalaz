package scalaz

trait Pointed[F[_]] {
  def point[A](a: => A): F[A]

  def **[G[_]: Pointed]: Pointed[({type λ[α]=(F[α], G[α])})#λ] =
    new Pointed[({type λ[α]=(F[α], G[α])})#λ] {
      def point[A](a: => A) =
        (Pointed.this.point(a), implicitly[Pointed[G]].point(a))
    }

  def deriving[G[_]](implicit n: ^**^[G, F]): Pointed[G] =
    new Pointed[G] {
      def point[A](a: => A) =
        n.pack(Pointed.this.point(a))
    }

}

object Pointed extends Pointeds

trait Pointeds extends PointedsLow {

  import java.util.concurrent.Callable

  implicit val OptionPointed: Pointed[Option] = new Pointed[Option] {
    def point[A](a: => A) = Some(a)
  }

  implicit val ListPointed: Pointed[List] = new Pointed[List] {
    def point[A](a: => A) = List(a)
  }

  implicit val StreamPointed: Pointed[Stream] = new Pointed[Stream] {
    def point[A](a: => A) = Stream(a)
  }

  implicit def CallablePointed: Pointed[Callable] = new Pointed[Callable] {
    def point[A](a: => A) = new Callable[A] {
      def call = a
    }
  }
}

trait PointedsLow {
  import collection.TraversableLike

  implicit def TraversablePointed[CC[X] <: TraversableLike[X, CC[X]] : CanBuildAnySelf]: Pointed[CC] = new Pointed[CC] {
    def point[A](a: => A) = {
      val builder = implicitly[CanBuildAnySelf[CC]].apply[Nothing, A]
      builder += a
      builder.result
    }
  }

}