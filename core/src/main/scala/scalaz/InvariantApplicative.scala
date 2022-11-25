package scalaz

////
/**
 * Invariant parent of Divisible and Applicative.
 *
 * Used for typeclass derivation of products and value types.
 */
////
trait InvariantApplicative[F[_]] extends InvariantFunctor[F] { self =>
  ////

  def xproduct0[Z](f: =>Z): F[Z]
  def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    xmap(a1, f, g)

  def xproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z,
    g: Z => (A1, A2)
  ): F[Z]
  def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = {
    val a23: F[(A2, A3)] = xproduct2(a2, a3)(
      (a, b) => (a, b),
      v => v
    )
    val a123: F[(A1, A2, A3)] = xproduct2(a1, a23)(
      (a, b) => (a, b._1, b._2),
      v => (v._1, (v._2, v._3))
    )
    xmap(a123, f.tupled, g)
  }

  def xproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  ): F[Z] = {
    val a12: F[(A1, A2)] = xproduct2(a1, a2)(
      (a, b) => (a, b),
      v => v
    )
    val a34: F[(A3, A4)] = xproduct2(a3, a4)(
      (a, b) => (a, b),
      v => v
    )
    val a1234: F[(A1, A2, A3, A4)] = xproduct2(a12, a34)(
      (a, b) => (a._1, a._2, b._1, b._2),
      v => ((v._1, v._2), (v._3, v._4))
    )
    xmap(a1234, f.tupled, g)
  }

  // these methods fail for recursive ADTs, fixed in
  // https://github.com/scala/scala/pull/6050
  final def xderiving0[Z](z: =>Z): F[Z] = xproduct0(z)
  final def xderiving1[Z, A1](
    f: A1 => Z,
    g: Z => A1
  )(implicit a1: F[A1]): F[Z] = xproduct1(a1)(f, g)
  final def xderiving2[Z, A1, A2](
    f: (A1, A2) => Z,
    g: Z => (A1, A2)
  )(implicit a1: F[A1], a2: F[A2]): F[Z] = xproduct2(a1, a2)(f, g)
  final def xderiving3[Z, A1, A2, A3](
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3]): F[Z] = xproduct3(a1, a2, a3)(f, g)
  final def xderiving4[Z, A1, A2, A3, A4](
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[Z] = xproduct4(a1, a2, a3, a4)(f, g)

  ////
  val invariantApplicativeSyntax: scalaz.syntax.InvariantApplicativeSyntax[F] =
    new scalaz.syntax.InvariantApplicativeSyntax[F] { def F = InvariantApplicative.this }
}

object InvariantApplicative {
  @inline def apply[F[_]](implicit F: InvariantApplicative[F]): InvariantApplicative[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: InvariantApplicative[G]): InvariantApplicative[F] =
    new IsomorphismInvariantApplicative[F, G] {
      override def G: InvariantApplicative[G] = E
      override def iso: F <~> G = D
    }

  ////

  implicit def idInstance: InvariantApplicative[Id.Id] = Id.id
  ////
}

trait IsomorphismInvariantApplicative[F[_], G[_]] extends InvariantApplicative[F] with IsomorphismInvariantFunctor[F, G]{
  implicit def G: InvariantApplicative[G]
  ////
  def xproduct0[Z](f: => Z): F[Z] =
    iso.from(G.xproduct0(f))
  def xproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => Z, g: Z => (A1, A2)): F[Z] =
    iso.from(G.xproduct2(iso.to(a1), iso.to(a2))(f, g))
  ////
}
