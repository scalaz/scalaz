package scalaz

trait Contravariant[F[_]] {
  def contramap[A, B](f: B => A): F[A] => F[B]
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

  implicit def Function1Contravariant[X]: Contravariant[({type λ[α]=(α) => X})#λ] = new Contravariant[({type λ[α]=(α) => X})#λ] {
    def contramap[A, B](r: A => X, f: B => A) = r compose f
  }

  implicit def EqualContravariant: Contravariant[Equal] = new Contravariant[Equal] {
    def contramap[A, B](r: Equal[A], f: B => A) = equal[B]((b1, b2) => r equal (f(b1), f(b2)))
  }

  implicit def OrderContravariant: Contravariant[Order] = new Contravariant[Order] {
    def contramap[A, B](r: Order[A], f: B => A) = order[B]((b1, b2) => r order (f(b1), f(b2)))
  }

  implicit def OrderingContravariant: Contravariant[scala.Ordering] = new Contravariant[scala.Ordering] {
    def contramap[A, B](r: scala.Ordering[A], f: B => A) = r.on(f)
  }

  implicit def ShowContravariant: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](r: Show[A], f: B => A) =
      show[B](b => r show (f(b)))
  }

  implicit def MetricSpaceContravariant: Contravariant[MetricSpace] = new Contravariant[MetricSpace] {
    def contramap[A, B](r: MetricSpace[A], f: B => A) = metricSpace[B]((b1, b2) => r distance (f(b1), f(b2)))
  }

}
