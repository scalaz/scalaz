package scalaz

/**
 * Contra-variant function application in an environment.
 *
 * <p>
 * All contra-variant functor instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == comap(a, identity)</code></li>
 * <li><strong>composition</strong><br/><code>forall a f g. comap(a, f compose g) == comap(comap(a, f), g)</code></li>
 * </p>
 */
trait Cofunctor[F[_]] extends InvariantFunctor[F] {
  def comap[A, B](r: F[A], f: B => A): F[B]
  
  final def xmap[A,B](ma: F[A], f: A => B, g: B => A): F[B] = comap(ma, g)
}

object Cofunctor {
  import Scalaz._

  implicit def Function1Cofunctor[X]: Cofunctor[({type λ[α]=(α) => X})#λ] = new Cofunctor[({type λ[α]=(α) => X})#λ] {
    def comap[A, B](r: A => X, f: B => A) = r compose f
  }

  implicit def EqualCofunctor: Cofunctor[Equal] = new Cofunctor[Equal] {
    def comap[A, B](r: Equal[A], f: B => A) = equal[B]((b1, b2) => r equal (f(b1), f(b2)))
  }

  implicit def OrderCofunctor: Cofunctor[Order] = new Cofunctor[Order] {
    def comap[A, B](r: Order[A], f: B => A) = order[B]((b1, b2) => r order (f(b1), f(b2)))
  }

  implicit def OrderingCofunctor: Cofunctor[scala.Ordering] = new Cofunctor[scala.Ordering] {
    def comap[A, B](r: scala.Ordering[A], f: B => A) = r.on(f)
  }

  implicit def ShowCofunctor: Cofunctor[Show] = new Cofunctor[Show] {
    def comap[A, B](r: Show[A], f: B => A) = {
      if(r == null) error("boo")
      show[B](b => r show (f(b)))
    }
  }

  implicit def MetricSpaceCofunctor: Cofunctor[MetricSpace] = new Cofunctor[MetricSpace] {
    def comap[A, B](r: MetricSpace[A], f: B => A) = metricSpace[B]((b1, b2) => r distance (f(b1), f(b2)))
  }

  import concurrent.{Actor, Effect}

  implicit def ActorCofunctor: Cofunctor[Actor] = new Cofunctor[Actor] {
    def comap[A, B](r: Actor[A], f: B => A): Actor[B] = actor[B](r.onError, (b: B) => (r ! f(b))())(r.strategy)
  }

  implicit def EffectCofunctor: Cofunctor[Effect] = new Cofunctor[Effect] {
    def comap[A, B](r: Effect[A], f: B => A) = effect[B]((b) => r ! f(b))(r.strategy)
  }

  import java.util.Comparator

  implicit def ComparatorCofunctor: Cofunctor[Comparator] = new Cofunctor[Comparator] {
    def comap[A, B](r: Comparator[A], f: B => A) = new Comparator[B] {
      def compare(b1: B, b2: B) = r.compare(f(b1), f(b2))
    }
  }

  implicit def ComparableCofunctor: Cofunctor[Comparable] = new Cofunctor[Comparable] {
    def comap[A, B](r: Comparable[A], f: B => A) = new Comparable[B] {
      def compareTo(b: B) = r.compareTo(f(b))
    }
  }
}
