package scalaz
package typeclass

import scalaz.Prelude._
import scalaz.data.Disjunction.{-\/, \/-}
import scalaz.data.{As, Inhabited, Void}

final class IsInvariant[F[_]](proof: F[Void] >!< F[Any]) { F =>
  def apply[A, B](implicit ev: A =!= B): F[A] >!< F[B] =
    Inhabited.lem[F[A] === F[B]].flatMap {
      case \/-(equal) =>
        ev(injective(equal))

      case -\/(neq) =>
        Axioms.subtyping(neq).flatMap {
          case \/-(ncmp) => Inhabited.value(ncmp)
          case -\/(frel) =>

            Axioms.subtyping(ev).map {
              case \/-(ncmp) => injective.incomparable(ncmp)
              case -\/(rel) =>
                (frel, rel) match {
                  case (\/-(flt), \/-(lt)) =>
                    proof.notLessOrEqual(IsCovariant.witness[F, A, B](lt, flt.conformity).apply(As.bottomTop))
                  case (-\/(fgt), -\/(gt)) =>
                    proof.notLessOrEqual(IsCovariant.witness[F, B, A](gt, fgt.conformity).apply(As.bottomTop))

                  case (\/-(flt), -\/(gt)) =>
                    proof.notGreaterOrEqual(IsContravariant.witness[F, B, A](gt, flt.conformity).apply(As.bottomTop))
                  case (-\/(fgt), \/-(lt)) =>
                    proof.notGreaterOrEqual(IsContravariant.witness[F, A, B](lt, fgt.conformity).apply(As.bottomTop))
                }
            }
        }
    }.proved

  def injective: IsInjective[F] =
    IsInjective.witness[F](F(Void.isNotAny).notEqual)

  def composePh[G[_]](G: IsConstant[G]): IsConstant[λ[x => F[G[x]]]] =
    IsConstant.witness[λ[x => F[G[x]]]](G[Void, Any].lift[F])

  def composeInj[G[_]](G: IsInjective[G]): IsInvariant[λ[x => F[G[x]]]] =
    IsInvariant.witness[λ[x => F[G[x]]]](F(G.contrapositive(Void.isNotAny)))

  def composeStCv[G[_]](G: IsStrictlyCovariant[G]): IsInvariant[λ[x => F[G[x]]]] =
    IsInvariant.witness[λ[x => F[G[x]]]](F(G.injective.contrapositive(Void.isNotAny)))

  def composeStCt[G[_]](G: IsStrictlyContravariant[G]): IsInvariant[λ[x => F[G[x]]]] =
    IsInvariant.witness[λ[x => F[G[x]]]](F(G.injective.contrapositive(Void.isNotAny)))

  def composeInv [G[_]](G: IsInvariant[G]): IsInvariant[λ[x => F[G[x]]]] =
    IsInvariant.witness[λ[x => F[G[x]]]](F(G(Void.isNotAny).notEqual))

}
object IsInvariant {
  def witness[F[_]](proof: F[Void] >!< F[Any]): IsInvariant[F] =
    new IsInvariant[F](proof)
}