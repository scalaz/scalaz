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
trait ContraFunctor[F[_]] extends InvariantFunctor[F] {
  def contramap[A, B](r: F[A], f: B => A): F[B]
  
  final def xmap[A,B](ma: F[A], f: A => B, g: B => A): F[B] = contramap(ma, g)
}

object ContraFunctor {
  import Scalaz._

  implicit def Function1Cofunctor[X]: ContraFunctor[({type λ[α]=(α) => X})#λ] = new ContraFunctor[({type λ[α]=(α) => X})#λ] {
    def contramap[A, B](r: A => X, f: B => A) = r compose f
  }

  implicit def EqualCofunctor: ContraFunctor[Equal] = new ContraFunctor[Equal] {
    def contramap[A, B](r: Equal[A], f: B => A) = equal[B]((b1, b2) => r equal (f(b1), f(b2)))
  }

  implicit def OrderCofunctor: ContraFunctor[Order] = new ContraFunctor[Order] {
    def contramap[A, B](r: Order[A], f: B => A) = order[B]((b1, b2) => r order (f(b1), f(b2)))
  }

  implicit def OrderingCofunctor: ContraFunctor[scala.Ordering] = new ContraFunctor[scala.Ordering] {
    def contramap[A, B](r: scala.Ordering[A], f: B => A) = r.on(f)
  }

  implicit def ShowCofunctor: ContraFunctor[Show] = new ContraFunctor[Show] {
    def contramap[A, B](r: Show[A], f: B => A) = {
      if(r == null) error("boo")
      show[B](b => r show (f(b)))
    }
  }

  implicit def MetricSpaceCofunctor: ContraFunctor[MetricSpace] = new ContraFunctor[MetricSpace] {
    def contramap[A, B](r: MetricSpace[A], f: B => A) = metricSpace[B]((b1, b2) => r distance (f(b1), f(b2)))
  }

  import concurrent.{Actor, Effect}

  implicit def ActorCofunctor: ContraFunctor[Actor] = new ContraFunctor[Actor] {
    def contramap[A, B](r: Actor[A], f: B => A): Actor[B] = actor[B]((b: B) => (r ! f(b))(), r.onError)(r.strategy)
  }

  implicit def EffectCofunctor: ContraFunctor[Effect] = new ContraFunctor[Effect] {
    def contramap[A, B](r: Effect[A], f: B => A) = effect[B]((b) => r ! f(b))(r.strategy)
  }

  import java.util.Comparator

  implicit def ComparatorCofunctor: ContraFunctor[Comparator] = new ContraFunctor[Comparator] {
    def contramap[A, B](r: Comparator[A], f: B => A) = new Comparator[B] {
      def compare(b1: B, b2: B) = r.compare(f(b1), f(b2))
    }
  }

  implicit def ComparableCofunctor: ContraFunctor[Comparable] = new ContraFunctor[Comparable] {
    def contramap[A, B](r: Comparable[A], f: B => A) = new Comparable[B] {
      def compareTo(b: B) = r.compareTo(f(b))
    }
  }
}
