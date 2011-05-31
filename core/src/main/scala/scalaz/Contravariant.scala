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

  implicit def ComparatorContravariant: Contravariant[Comparator] = new Contravariant[Comparator] {
    def contramap[A, B](f: B => A) =
      r => new Comparator[B] {
        def compare(b1: B, b2: B) = r.compare(f(b1), f(b2))
      }
  }

  implicit def ComparableContravariant: Contravariant[Comparable] = new Contravariant[Comparable] {
    def contramap[A, B](f: B => A) =
      r => new Comparable[B] {
        def compareTo(b: B) = r.compareTo(f(b))
      }
  }

}