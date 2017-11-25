package scalaz
package typeclass

import scalaz.Prelude._
import scalaz.data.Disjunction.{-\/, \/-}
import scalaz.data.{As, Inhabited, StrictAs}

/**
 * Witnesses that the type constructor `F[_]` is covariant,
 * even though the variance annotation of its type parameter has been forgotten.
 *
 * A safer alternative to
 * https://typelevel.org/blog/2014/03/09/liskov_lifting.html
 */
final class IsCovariant[F[_]](proof: ¬¬[IsConstant[F] \/ IsStrictlyCovariant[F]]) { F =>
  import IsCovariant._

  private[this] def strict[A, B](ev: A </< B): ¬¬[F[A] <~< F[B]] = proof.map {
    case \/-(cv) => cv(ev).conformity
    case -\/(ph) => ph[A, B].toAs
  }

  def apply[A, B](implicit ev: A <~< B): F[A] <~< F[B] = ev.decompose[A, B].flatMap {
    case \/-(equal) => Inhabited.value(equal.lift[F].toAs)
    case -\/(less) => strict(less)
  }.proved[F[A] <~< F[B]]

  def substCv[G[+_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] =
    F(ev).substCv[G](g)

  def substCt[G[-_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] =
    F(ev).substCt[G](g)

  def composePh[G[_]](G: IsConstant[G]): IsConstant[λ[x => F[G[x]]]] =
    G.andThenCo[F](F)

  def composeCv[G[_]](G: IsCovariant[G]): IsCovariant[λ[x => F[G[x]]]] =
    witness1[λ[x => F[G[x]]]](F(G(As.bottomTop)))

  def composeCt[G[_]](G: IsContravariant[G]): IsContravariant[λ[x => F[G[x]]]] =
    IsContravariant.witness1[λ[x => F[G[x]]]](F(G(As.bottomTop)))

  def composeStCv[G[_]](G: IsStrictlyCovariant[G]): IsCovariant[λ[x => F[G[x]]]] =
    F composeCv G.covariant

  def composeStCt[G[_]](G: IsStrictlyContravariant[G]): IsContravariant[λ[x => F[G[x]]]] =
    F composeCt G.contravariant

  def composeInv[G[_]](G: IsInvariant[G]): ¬¬[IsConstant[F] \/ IsInvariant[λ[x => F[G[x]]]]] =
    F.decompose.map {
      case \/-(scv) => \/-(scv.composeInv(G))
      case -\/(const) => -\/(const)
    }

  def decompose: ¬¬[IsConstant[F] \/ IsStrictlyCovariant[F]] =
    proof
}

object IsCovariant {
  def apply[F[_]](implicit F: IsCovariant[F]): IsCovariant[F] = F

  def witness[F[_], A, B](implicit p: A </< B, q: F[A] <~< F[B]): IsCovariant[F] = {
    def injective(inj: IsInjective[F]): ¬¬[IsStrictlyCovariant[F]] =
      inj.decompose.map {
        case \/-(inv) => inv[A, B](p.inequality[A, B]).notLessOrEqual(q)
        case -\/(variance) =>
          variance match {
            case \/-(cv) => cv
            case -\/(ct) => ct[A, B](p).contradicts(q)
          }
      }

    val proof : ¬¬[IsConstant[F] \/ IsStrictlyCovariant[F]] =
      Axioms.parametricity[F].flatMap {
        case \/-(inj) => injective(inj).map(\/-.apply)
        case -\/(ph) => Inhabited.value(-\/(ph))
      }

    new IsCovariant[F](proof)
  }

  def witness1[F[_]](implicit ev: F[Void] <~< F[Any]): IsCovariant[F] =
    witness(StrictAs.bottomTop, ev)

  def witness2[F[_]](proof: ¬¬[IsConstant[F] \/ IsStrictlyCovariant[F]]): IsCovariant[F] =
    new IsCovariant[F](proof)

  implicit def reify[F[+_]]: IsCovariant[F] = witness1[F](As[F[Void], F[Any]])

  implicit def id: IsCovariant[λ[x => x]] = {
    type f[+x] = x
    reify[f]
  }
}
