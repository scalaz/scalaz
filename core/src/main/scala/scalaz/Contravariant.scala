package scalaz

/**
 * Contra-variant function application in an environment.
 *
 * <p>
 * All contra-variant functor instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == contramap(a, identity)</code></li>
 * <li><strong>composition</strong><br/><code>forall a f g. contramap(a, f compose g) == contramap(contramap(a, f), g)</code></li>
 * </p>
 */
trait Contravariant[F[_]] extends InvariantFunctor[F] {
  def contramap[A, B](r: F[A], f: B => A): F[B]
  
  final def xmap[A,B](ma: F[A], f: A => B, g: B => A): F[B] = contramap(ma, g)
}

object Contravariant {
  import Scalaz._

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
    def contramap[A, B](r: Show[A], f: B => A) = {
      if(r == null) error("boo")
      show[B](b => r show (f(b)))
    }
  }

  implicit def MetricSpaceContravariant: Contravariant[MetricSpace] = new Contravariant[MetricSpace] {
    def contramap[A, B](r: MetricSpace[A], f: B => A) = metricSpace[B]((b1, b2) => r distance (f(b1), f(b2)))
  }

  import concurrent.{Actor, Effect}

  implicit def ActorContravariant: Contravariant[Actor] = new Contravariant[Actor] {
    def contramap[A, B](r: Actor[A], f: B => A): Actor[B] = actor[B]((b: B) => (r ! f(b))(), r.onError)(r.strategy)
  }

  implicit def EffectContravariant: Contravariant[Effect] = new Contravariant[Effect] {
    def contramap[A, B](r: Effect[A], f: B => A) = effect[B]((b) => r ! f(b))(r.strategy)
  }

  import java.util.Comparator

  implicit def ComparatorContravariant: Contravariant[Comparator] = new Contravariant[Comparator] {
    def contramap[A, B](r: Comparator[A], f: B => A) = new Comparator[B] {
      def compare(b1: B, b2: B) = r.compare(f(b1), f(b2))
    }
  }

  implicit def ComparableContravariant: Contravariant[Comparable] = new Contravariant[Comparable] {
    def contramap[A, B](r: Comparable[A], f: B => A) = new Comparable[B] {
      def compareTo(b: B) = r.compareTo(f(b))
    }
  }
}
