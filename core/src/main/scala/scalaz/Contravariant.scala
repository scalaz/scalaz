package scalaz


trait Contravariant[F[_]] {
  def contramap[A, B](f: B => A): F[A] => F[B]
}

object Contravariant extends Contravariants

trait Contravariants {

  implicit def Function1Contravariant[X]: Contravariant[({type λ[α] = (α) => X})#λ] = new Contravariant[({type λ[α] = (α) => X})#λ] {
    def contramap[A, B](f: B => A) =
      _ compose f
  }

  import java.util.Comparator

  implicit val ComparatorContravariant: Contravariant[Comparator] = new Contravariant[Comparator] {
    def contramap[A, B](f: B => A) =
      r => new Comparator[B] {
        def compare(b1: B, b2: B) = r.compare(f(b1), f(b2))
      }
  }

  implicit val ComparableContravariant: Contravariant[Comparable] = new Contravariant[Comparable] {
    def contramap[A, B](f: B => A) =
      r => new Comparable[B] {
        def compareTo(b: B) = r.compareTo(f(b))
      }
  }

  implicit def KleisliContravariant[F[_], X]: Contravariant[({type λ[α] = Kleisli[α, F, X]})#λ] = new Contravariant[({type λ[α] = Kleisli[α, F, X]})#λ] {
    def contramap[A, B](f: B => A) =
      _ contramapRead f
  }

  implicit val MetricSpaceContravariant: Contravariant[MetricSpace] = new Contravariant[MetricSpace] {
    def contramap[A, B](f: B => A) =
      _ contramap f
  }

  implicit val EqualContravariant: Contravariant[Equal] = new Contravariant[Equal] {
    def contramap[A, B](f: B => A) =
      _ contramap f
  }

  implicit val OrderContravariant: Contravariant[Order] = new Contravariant[Order] {
    def contramap[A, B](f: B => A) =
      _ contramap f
  }

  implicit val ShowContravariant: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](f: B => A) =
      _ contramap f
  }

  implicit val ResourceContravariant: Contravariant[Resource] = new Contravariant[Resource] {
    def contramap[A, B](f: B => A) =
      _ contramap f
  }

}