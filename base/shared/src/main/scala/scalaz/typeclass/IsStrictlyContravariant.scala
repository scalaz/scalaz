package scalaz
package typeclass

import scalaz.Prelude._
import scalaz.data.Disjunction.{-\/, \/-}
import scalaz.data.{As, StrictAs, Void}

final class IsStrictlyContravariant[F[_]]
(val contravariant: IsContravariant[F],
 val injective: IsInjective[F]
) { F =>

  def apply[A, B](ev: A </< B): F[B] </< F[A] =
    StrictAs.witness[F[B], F[A]](
      x => injective.contrapositive(ev.inequality[A, B])(x.flip),
      contravariant[A, B](ev.conformity))

  def inverse[A, B](ev: F[B] </< F[A]): A </< B = {
    val nfab: F[B] =!= F[A] = ev.inequality[F[B], F[A]]
    val nab: A =!= B = ab => nfab(ab.flip.lift[F])
    val cfab: F[B] <~< F[A] = ev.conformity

    val cab : A <~< B = Axioms.subtyping(nab).map {
      case \/-(ncmp) => injective.incomparable(ncmp).notGreater(ev)
      case -\/(\/-(lt)) => lt.conformity
      case -\/(-\/(gt)) => F(gt).contradicts(cfab)
    }.proved

    StrictAs.witness[A, B](nab, cab)
  }

  def nonStrict[A, B](ev: A <~< B): F[B] <~< F[A] = ev.decompose[A, B].map {
    case \/-(equal) => equal.flip.lift[F].toAs
    case -\/(less) => F(less).conformity
  }.proved

  def nonStrictContrapositive[A, B](ev: ¬[F[B] <~< F[A]]): ¬[A <~< B] =
    ab => ev(nonStrict(ab))

  def composePh[G[_]](G: IsConstant[G]): IsConstant[λ[x => F[G[x]]]] = {
    IsConstant.witness[λ[x => F[G[x]]]](
      As.bracket(
        F.nonStrict(G[Any, Void].toAs),
        F.nonStrict(G[Void, Any].toAs)))
  }

  def composeInj[G[_]](G: IsInjective[G]): IsInjective[λ[x => F[G[x]]]] =
    F.injective compose G

  def composeCv[G[_]](G: IsCovariant[G]): IsContravariant[λ[x => F[G[x]]]] =
    F.contravariant composeCv G

  def composeStCv[G[_]](G: IsStrictlyCovariant[G]): IsStrictlyContravariant[λ[x => F[G[x]]]] =
    IsStrictlyContravariant.witness(
      F.contravariant composeCv G.covariant,
      F.injective compose G.injective)

  def composeStCt[G[_]](G: IsStrictlyContravariant[G]): IsStrictlyCovariant[λ[x => F[G[x]]]] =
    IsStrictlyCovariant.witness(
      F.contravariant composeCt G.contravariant,
      F.injective compose G.injective)

  def composeInv [G[_]](G: IsInvariant[G]): IsInvariant[λ[x => F[G[x]]]] =
    IsInvariant.witness[λ[x => F[G[x]]]](injective.incomparable(G(Void.isNotAny)))
}
object IsStrictlyContravariant {
  def witness[F[_]](contravariant: IsContravariant[F], injective: IsInjective[F]): IsStrictlyContravariant[F] =
    new IsStrictlyContravariant[F](contravariant, injective)

  def witnessNot[F[_], A, B](p: A </< B, q: ¬[F[B] </< F[A]]): IsStrictlyContravariant[F] => Void =
    F => q(F(p))
}