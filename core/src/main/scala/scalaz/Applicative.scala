package scalaz

/**
 * Defines an applicative functor as described by McBride and Paterson in
 * <a href="http://www.soi.city.ac.uk/~ross/papers/Applicative.html">Applicative Programming with Effects</a>.
 *
 * <p>
 * All instances must satisfy 4 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == apply(a, pure(identity))</code></li>
 * <li><strong>composition</strong><br/><code>forall af ag a. apply(apply(a, ag), af) == apply(a, apply(ag, apply(af, pure(compose))))</code></li>
 * <li><strong>homomorphism</strong><br/><code>forall f a. apply(pure(a), pure(f)) == pure(f(a))</code></li>
 * <li><strong>interchange</strong><br/><code>forall af a. apply(pure(a), af) == apply(af, pure(f => f(x)))</code></li>
 * </ol>
 * </p>
 */
trait Applicative[Z[_]] extends Pointed[Z] with Apply[Z] {
  override def fmap[A, B](fa: Z[A], f: A => B): Z[B] = this(pure(f), fa)
}

object Applicative {
  implicit def applicative[Z[_]](implicit p: Pure[Z], a: Apply[Z]): Applicative[Z] = new Applicative[Z] {
    def pure[A](a: => A) = p.pure(a)
    def apply[A, B](f: => Z[A => B], x: => Z[A]) = a(f, x)
  }

  import Pure._
  import Apply._
  import Scalaz._

  implicit def MonoidalApplicative[B: Monoid]: Applicative[({type λ[α]=Const[B, α]})#λ]
      = applicative[({type λ[α]=Const[B, α]})#λ](ConstPure, ConstApply)
}
