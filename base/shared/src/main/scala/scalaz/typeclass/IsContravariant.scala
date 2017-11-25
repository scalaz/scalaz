package scalaz
package typeclass

import Prelude._
import data.{As, Inhabited, StrictAs, Void}
import scalaz.data.Disjunction.{-\/, \/-}

/**
 * Witnesses that the type constructor `F[_]` is contravariant,
 * even though the variance annotation of its type parameter has been forgotten.
 *
 * A safer alternative to
 * https://typelevel.org/blog/2014/03/09/liskov_lifting.html
 */
final class IsContravariant[F[_]](proof: ¬¬[IsConstant[F] \/ IsStrictlyContravariant[F]]) { F =>
  import IsContravariant._

  private[this] def strict[A, B](ev: A </< B): ¬¬[F[B] <~< F[A]] = proof.map {
    case \/-(ct) => ct(ev).conformity
    case -\/(ph) => ph[B, A].toAs
  }

  def apply[A, B](implicit ev: A <~< B): F[B] <~< F[A] = ev.decompose[A, B].flatMap {
    case \/-(equal) => Inhabited.value(equal.lift[F].flip.toAs)
    case -\/(less) => strict(less)
  }.proved

  def substCv[G[+_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] =
    F(ev).substCv(g)

  def substCt[G[-_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] =
    F(ev).substCt(g)

  def composePh[G[_]](G: IsConstant[G]): IsConstant[λ[x => F[G[x]]]] =
    G.andThenCt[F](F)

  def composeCv[G[_]](G: IsCovariant[G]): IsContravariant[λ[x => F[G[x]]]] =
    witness1[λ[x => F[G[x]]]](F(G(As.bottomTop)))

  def composeCt[G[_]](G: IsContravariant[G]): IsCovariant[λ[x => F[G[x]]]] =
    IsCovariant.witness1[λ[x => F[G[x]]]](F(G(As.bottomTop)))

  def composeInv[G[_]](G: IsInvariant[G]): ¬¬[IsConstant[F] \/ IsInvariant[λ[x => F[G[x]]]]] =
    F.decompose.map {
      case \/-(scv) => \/-(scv.composeInv(G))
      case -\/(const) => -\/(const)
    }

  def decompose: ¬¬[IsConstant[F] \/ IsStrictlyContravariant[F]] = proof
}

object IsContravariant {
  def apply[F[_]](implicit ev: IsContravariant[F]): IsContravariant[F] = ev

  def witness[F[_], A, B](implicit p: A </< B, q: F[B] <~< F[A]): IsContravariant[F] = {
    def injective(inj: IsInjective[F]): ¬¬[IsStrictlyContravariant[F]] =
      inj.decompose.map {
        case \/-(inv) => inv[A, B](p.inequality[A, B]).notGreaterOrEqual(q)
        case -\/(variance) =>
          variance match {
            case \/-(cv) => cv[A, B](p).contradicts(q)
            case -\/(ct) => ct
          }
      }

    val proof : ¬¬[IsConstant[F] \/ IsStrictlyContravariant[F]] =
      Axioms.parametricity[F].flatMap {
        case \/-(inj) => injective(inj).map(\/-.apply)
        case -\/(ph) => Inhabited.value(-\/(ph))
      }

    new IsContravariant(proof)
  }

  def witness1[F[_]](implicit ev: F[Any] <~< F[Void]): IsContravariant[F] =
    witness[F, Void, Any](StrictAs.bottomTop, ev)

  def witness2[F[_]](ev: ¬¬[IsConstant[F] \/ IsStrictlyContravariant[F]]): IsContravariant[F] =
    new IsContravariant[F](ev)

  implicit def reify[F[-_]]: IsContravariant[F] = witness1[F](As[F[Any], F[Void]])

  implicit val void: IsContravariant[λ[x => (x => Void)]] = {
    type f[-x] = x => Void
    reify[f]
  }
}
