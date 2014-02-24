package scalaz

/**
 * The dual view of the Yoneda lemma. Also a free functor on `F`.
 * This is isomorphic to `F` as long as `F` itself is a functor.
 * The homomorphism from `F[A]` to `Coyoneda[F,A]` exists even when
 * `F` is not a functor.
 */
abstract class Coyoneda[F[_], A] { coyo =>
  type I
  def fi: F[I]
  def k(i: I): A

  /** Converts to `F[A]` given that `F` is a functor */
  def run(implicit F: Functor[F]): F[A] =
    F.map(fi)(k)

  /** Converts to `Yoneda[F,A]` given that `F` is a functor */
  def toYoneda(implicit F: Functor[F]): Yoneda[F, A] = new Yoneda[F, A] {
    def apply[B](f: A => B) = F.map(fi)(k _ andThen f)
  }

  /** Simple function composition. Allows map fusion without touching the underlying `F`. */
  def map[B](f: A => B): Coyoneda[F, B] = new Coyoneda[F, B] {
    type I = coyo.I
    val fi = coyo.fi
    def k(i: I) = f(coyo k i)
  }

  import Id._

  /** `Coyoneda[F,_]` is the left Kan extension of `F` along `Id` */
  def toLan: Lan[Id, F, A] = new Lan[Id, F, A] {
    type I = coyo.I
    val v = fi
    def f(i: I) = k(i)
  }
}

object Coyoneda {

  /** `F[A]` converts to `Coyoneda[F,A]` for any `F` */
  def apply[F[_],A](fa: F[A]) = new Coyoneda[F,A] {
    type I = A
    def k(a: A) = a
    val fi = fa
  }

  /** `Coyoneda[F,_]` is a functor for any `F` */
  implicit def coyonedaFunctor[F[_]]: Functor[({type λ[α] = Coyoneda[F,α]})#λ] =
    new Functor[({type λ[α] = Coyoneda[F,α]})#λ] {
      def map[A,B](ya: Coyoneda[F,A])(f: A => B) = ya map f
    }

}
