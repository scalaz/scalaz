package scalaz
package typeclass

import scalaz.Prelude._
import scalaz.data.Disjunction.{-\/, \/-}
import scalaz.data.{As, StrictAs, Void}

final class IsStrictlyCovariant[F[_]]
(val covariant: IsCovariant[F],
 val injective: IsInjective[F]
) { F =>

  def apply[A, B](ev: A </< B): F[A] </< F[B] =
    StrictAs.witness[F[A], F[B]](
      injective.contrapositive(ev.inequality[A, B]),
      covariant[A, B](ev.conformity))

  def inverse[A, B](ev: F[A] </< F[B]): A </< B = {
    val nfab: F[A] =!= F[B] = ev.inequality[F[A], F[B]]
    val nab: A =!= B = ab => nfab(ab.lift[F])
    val cfab: F[A] <~< F[B] = ev.conformity

    val cab : A <~< B = Axioms.subtyping(nab).map {
      case \/-(ncmp) => injective.incomparable(ncmp).notLess(ev)
      case -\/(\/-(lt)) => lt.conformity
      case -\/(-\/(gt)) => F(gt).contradicts(cfab)
    }.proved

    StrictAs.witness[A, B](nab, cab)
  }

  def contrapositive[A, B](ev: ¬[F[A] </< F[B]]): ¬[A </< B] =
    ab => ev(F(ab))

  def nonStrict[A, B](ev: A <~< B): F[A] <~< F[B] = ev.decompose[A, B].map {
    case \/-(equal) => equal.lift[F].toAs
    case -\/(less) => F(less).conformity
  }.proved

  def nonStrictContrapositive[A, B](ev: ¬[F[A] <~< F[B]]): ¬[A <~< B] =
    ab => ev(nonStrict(ab))

  def composePh[G[_]](G: IsConstant[G]): IsConstant[λ[x => F[G[x]]]] = {
    IsConstant.witness[λ[x => F[G[x]]]](
      As.bracket(
        F.nonStrict(G[Void, Any].toAs),
        F.nonStrict(G[Any, Void].toAs)))
  }

  def composeInj[G[_]](G: IsInjective[G]): IsInjective[λ[x => F[G[x]]]] =
    F.injective compose G

  def composeCv[G[_]](G: IsCovariant[G]): IsCovariant[λ[x => F[G[x]]]] =
    F.covariant composeCv G

  def composeCt[G[_]](G: IsContravariant[G]): IsContravariant[λ[x => F[G[x]]]] =
    F.covariant composeCt G

  def composeStCv[G[_]](G: IsStrictlyCovariant[G]): IsStrictlyCovariant[λ[x => F[G[x]]]] =
    IsStrictlyCovariant.witness(
      F.covariant composeCv G.covariant,
      F.injective compose G.injective)

  def composeStCt[G[_]](G: IsStrictlyContravariant[G]): IsStrictlyContravariant[λ[x => F[G[x]]]] =
    IsStrictlyContravariant.witness(
      F.covariant composeCt G.contravariant,
      F.injective compose G.injective)

  def composeInv [G[_]](G: IsInvariant[G]): IsInvariant[λ[x => F[G[x]]]] =
    IsInvariant.witness[λ[x => F[G[x]]]](injective.incomparable(G(Void.isNotAny)))
}
object IsStrictlyCovariant {
  def witness[F[_]](covariant: IsCovariant[F], injective: IsInjective[F]): IsStrictlyCovariant[F] =
    new IsStrictlyCovariant[F](covariant, injective)

  /**
    * ¬(strictly-covariant f)
    * ⟺ ¬(∀ a b. a < b ⟶ f a < f b)
    * ⟺ ¬(∀ a b. ¬(a < b) ⋁ f a < f b)
    * ⟺ ∃ a b. a < b ⋀ ¬(f a < f b)
    */
  def witnessNot[F[_], A, B](p: A </< B, q: ¬[F[A] </< F[B]]): IsStrictlyCovariant[F] => Void =
    F => q(F(p))
}