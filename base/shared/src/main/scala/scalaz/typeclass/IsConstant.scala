package scalaz
package typeclass

import scalaz.Prelude._
import scalaz.data.Disjunction.{-\/, \/-}
import scalaz.data.{As, Is, Iso, Void}

final class IsConstant[F[_]](proof: F[Void] === F[Any]) { F =>
  import IsConstant._

  def apply[A, B]: F[A] === F[B] =
    Axioms.parametricity[F].map {
      case -\/(c) => c[A, B]
      case \/-(inj) => Void.isNotAny(inj(proof))
    }.proved

  def subst[G[_], A, B](g: G[F[A]]): G[F[B]] = F[A, B].subst[G](g)

  def compose[G[_]]: IsConstant[λ[x => F[G[x]]]] =
    witness[λ[x => F[G[x]]]](F[G[Void], G[Any]])

  def andThenCo[G[_]](implicit G: IsCovariant[G]): IsConstant[λ[x => G[F[x]]]] = {
    val r: G[F[Void]] === G[F[Any]] = As.bracket(G(F[Void, Any].toAs), G(F[Any, Void].toAs))
    witness[λ[x => G[F[x]]]](r)
  }

  def andThenCt[G[_]](implicit G: IsContravariant[G]): IsConstant[λ[x => G[F[x]]]] = {
    val r: G[F[Void]] === G[F[Any]] = As.bracket(G(F[Any, Void].toAs), G(F[Void, Any].toAs))
    witness[λ[x => G[F[x]]]](r)
  }

  def toCovariant: IsCovariant[F] =
    IsCovariant.witness1[F](apply[Void, Any].toAs)

  def toContravariant: IsContravariant[F] =
    IsContravariant.witness1[F](apply[Any, Void].toAs)
}
object IsConstant {
  def apply[F[_]](implicit ev: IsConstant[F]): IsConstant[F] = ev

  def witness[F[_]](fab: F[Void] === F[Any]): IsConstant[F] =
    new IsConstant[F](fab)

  def witness1[F[_], A, B](nab: A =!= B, fab: F[A] === F[B]): IsConstant[F] = {
    val p: F[Void] === F[Any] = Axioms.parametricity[F].map {
      case -\/(c) => c[Void, Any]
      case \/-(i) => nab(i(fab))
    }.proved

    witness[F](p)
  }

  implicit def const[A]: IsConstant[λ[X => A]] = witness[λ[X => A]](Is.refl[A])

  def bracket[F[_]](cv: IsCovariant[F], ct: IsContravariant[F]): IsConstant[F] =
    IsConstant.witness[F](Axioms.bracket(cv(As.bottomTop), ct(As.bottomTop)))

  implicit def proposition[F[_]]: Proposition[IsConstant[F]] =
    Proposition[F[Void] === F[Any]].isomap(Iso.unsafeTotal(p => witness[F](p), p => p[Void, Any]))
}