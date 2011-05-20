package scalaz


trait Contravariant[F[_]] {
  def contramap[A, B](f: B => A): F[A] => F[B]
}

object Contravariant extends Contravariants

trait Contravariants {

  import concurrent.{Actor, Run}
  import Equal._
  import Order._
  import Show._
  import MetricSpace._
  import Actor._
  import Run._

  implicit def Function1Contravariant[X]: Contravariant[({type λ[α]=(α) => X})#λ] = new Contravariant[({type λ[α]=(α) => X})#λ] {
    def contramap[A, B](f: B => A) =
      _ compose f
  }

  implicit def EqualContravariant: Contravariant[Equal] = new Contravariant[Equal] {
    def contramap[A, B](f: B => A) =
      r => equal[B](b1 => b2 => r.equal(f(b1))(f(b2)))
  }

  implicit def OrderContravariant: Contravariant[Order] = new Contravariant[Order] {
    def contramap[A, B](f: B => A) =
      r => order[B](b1 => b2 => r.order (f(b1))(f(b2)))
  }

  implicit def OrderingContravariant: Contravariant[scala.Ordering] = new Contravariant[scala.Ordering] {
    def contramap[A, B](f: B => A) =
      _ on f
  }

  implicit def ShowContravariant: Contravariant[Show] = new Contravariant[Show] {
    def contramap[A, B](f: B => A) =
      r => show[B](b => r show (f(b)))
  }

  implicit def MetricSpaceContravariant: Contravariant[MetricSpace] = new Contravariant[MetricSpace] {
    def contramap[A, B](f: B => A) =
      r => metricSpace[B](b1 => b2 => r.distance(f(b1))(f(b2)))
  }

  implicit def ActorContravariant: Contravariant[Actor] = new Contravariant[Actor] {
    def contramap[A, B](f: B => A) =
      r => actor[B]((b: B) => (r ! f(b))(), r.onError)(r.strategy)
  }

  implicit def RunContravariant: Contravariant[Run] = new Contravariant[Run] {
    def contramap[A, B](f: B => A) =
      r => run[B]((b) => r ! f(b))(r.strategy)
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