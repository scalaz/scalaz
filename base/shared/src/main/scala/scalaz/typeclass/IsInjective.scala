package scalaz
package typeclass

import scalaz.Prelude._
import scalaz.data.Disjunction.{-\/, \/-}
import scalaz.data.{Iso, Void}

final class IsInjective[F[_]](proof: F[Void] =!= F[Any]) { F =>
  import IsInjective._

  /**
    * The function f is said to be injective provided that for all a and b in X,
    * whenever f(a) = f(b), then a = b.
    */
  def apply[A, B](ev: F[A] === F[B]): A === B =
    Axioms.parametricity[F].map {
      case -\/(ph) => proof(ph[Void, Any])
      case \/-(inj) => inj(ev)
    }.proved

  /**
    * If A ≠ B, then F[A] ≠ F[B].
    */
  def contrapositive[A, B](ev: A =!= B): F[A] =!= F[B] =
    fab => ev(apply(fab))

  def incomparable[A, B](ev: A >!< B): F[A] >!< F[B] =
    Axioms.incomparable[F, A, B](ev).map {
      case \/-(ncmp) => ncmp
      case -\/(equal) => ev.notEqual(F(equal))
    }.proved

  /**
    * Constant type constructors are not injective.
    */
  def notConstant(constant: IsConstant[F]): Void =
    F.apply(constant.apply[Unit, Void]).coerce(())

  /**
    * If F and G are both injective, then F ∘ G is injective.
    */
  def compose[G[_]](implicit G: IsInjective[G]): IsInjective[λ[x => F[G[x]]]] =
    witness[λ[x => F[G[x]]]](F.contrapositive(G.contrapositive(Void.isNotAny)))

  /**
    * If F and G are both injective, then G ∘ F is injective.
    */
  def andThen[G[_]](implicit G: IsInjective[G]): IsInjective[λ[x => G[F[x]]]] =
    G compose F

  def decompose: ¬¬[IsStrictlyContravariant[F] \/ IsStrictlyCovariant[F] \/ IsInvariant[F]] =
    Axioms.subtypeParametricity(F)
}
object IsInjective {
  def apply[F[_]](implicit ev: IsInjective[F]): IsInjective[F] = ev

  def witness[F[_]](ev: F[Void] =!= F[Any]): IsInjective[F] =
    new IsInjective[F](ev)

  def witness1[F[_], A, B](nfab: F[A] =!= F[B]): IsInjective[F] =
    Axioms.parametricity[F].map {
      case -\/(ph) => nfab(ph[A, B])
      case \/-(inj) => inj
    }.proved

  def witness2[F[_], G[_], A, B](x: G[F[A]], y: G[F[B]] => Void): IsInjective[F] =
    witness1[F, A, B] { ab =>
      type f[x] = G[x]
      y(ab.subst[f](x))
    }

  implicit val id: IsInjective[λ[X => X]] = witness[λ[X => X]](Void.isNotAny)

  implicit def proposition[F[_]]: Proposition[IsInjective[F]] =
    Proposition[F[Void] =!= F[Any]].isomap(Iso.unsafeTotal(
      (p: F[Void] =!= F[Any]) => witness(p),
      (p: IsInjective[F]) => p.contrapositive(Void.isNotAny)))
}