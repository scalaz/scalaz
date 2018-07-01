package scalaz

////
// Copyright: 2018 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/**
 * https://hackage.haskell.org/package/semigroupoids-5.2.2/docs/Data-Functor-Alt.html
 */
////
trait Alt[F[_]] extends Applicative[F] with Derives[F] { self =>
  ////

  def alt[A](a1: =>F[A], a2: =>F[A]): F[A]

  /** One or none */
  def optional[A](fa: F[A]): F[Maybe[A]] = alt(map(fa)(Maybe.just(_)), pure(Maybe.empty))

  def altly1[Z, A1](a1: =>F[A1])(f: A1 => Z): F[Z] = map(a1)(f)
  def altly2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: A1 \/ A2 => Z): F[Z] =
    map(alt(map(a1)(-\/[A1, A2](_)), map(a2)(\/-[A1, A2](_))))(f)

  def altly3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: A1 \/ (A2 \/ A3) => Z
  ): F[Z] = altly2(a1, either2(a2, a3))(f)
  def altly4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3],a4: =>F[A4])(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  ): F[Z] = altly2(a1, either2(a2, either2(a3, a4)))(f)
  // ... altlyN

  // equivalent of tupleN
  def either2[A1, A2](a1: =>F[A1], a2: =>F[A2]): F[A1 \/ A2] =
    altly2(a1, a2)(identity)
  // ... eitherN

  final def altlying1[Z, A1](
    f: A1 => Z
  )(implicit a1: F[A1]): F[Z] =
    altly1(a1)(f)
  final def altlying2[Z, A1, A2](
    f: A1 \/ A2 => Z
  )(implicit a1: F[A1], a2: F[A2]): F[Z] =
    altly2(a1, a2)(f)
  final def altlying3[Z, A1, A2, A3](
    f: A1 \/ (A2 \/ A3) => Z
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3]): F[Z] =
    altly3(a1, a2, a3)(f)
  final def altlying4[Z, A1, A2, A3, A4](
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[Z] =
    altly4(a1, a2, a3, a4)(f)
  // ... altlyingX

  override def xproduct0[Z](z: =>Z): F[Z] = pure(z)
  override def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] = xmap(a1, f, g)
  override def xproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z, g: Z => (A1, A2)
  ): F[Z] = apply2(a1, a2)(f)
  override def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = apply3(a1, a2, a3)(f)
  override def xproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  ): F[Z] = apply4(a1, a2, a3, a4)(f)

  override def xcoproduct1[Z, A1](a1: =>F[A1])(
    f: A1 => Z,
    g: Z => A1
  ): F[Z] = altly1(a1)(f)
  override def xcoproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1 \/ A2) => Z,
    g: Z => (A1 \/ A2)
  ): F[Z] = altly2(a1, a2)(f)
  override def xcoproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1 \/ (A2 \/ A3)) => Z,
    g: Z => (A1 \/ (A2 \/ A3))
  ): F[Z] = altly3(a1, a2, a3)(f)
  override def xcoproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
    g: Z => (A1 \/ (A2 \/ (A3 \/ A4)))
  ): F[Z] = altly4(a1, a2, a3, a4)(f)

  trait AltLaw extends ApplicativeLaw {
    // TODO: laws

    // <!> is associative:             (a <!> b) <!> c = a <!> (b <!> c)
    // <$> left-distributes over <!>:  f <$> (a <!> b) = (f <$> a) <!> (f <$> b)
  }
  def altLaw = new AltLaw {}

  ////
  val altSyntax = new scalaz.syntax.AltSyntax[F] { def F = Alt.this }
}

object Alt {
  @inline def apply[F[_]](implicit F: Alt[F]): Alt[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Alt[G]): Alt[F] =
    new IsomorphismAlt[F, G] {
      override def G: Alt[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismAlt[F[_], G[_]] extends Alt[F] with IsomorphismApplicative[F, G] with IsomorphismDerives[F, G]{
  implicit def G: Alt[G]
  ////
  override def alt[A](a1: =>F[A], a2: =>F[A]): F[A] = iso.from(G.alt(iso.to(a1), iso.to(a2)))

  override def xproduct0[Z](z: => Z): F[Z] =
    super[Alt].xproduct0(z)

  override def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    super[Alt].xproduct1(a1)(f, g)
  override def xproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => Z, g: Z => (A1, A2)): F[Z] =
    super[Alt].xproduct2(a1, a2)(f, g)
  override def xproduct3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(f: (A1, A2, A3) => Z, g: Z => (A1, A2, A3)): F[Z] =
    super[Alt].xproduct3(a1, a2, a3)(f, g)
  override def xproduct4[Z, A1, A2, A3, A4](a1: => F[A1], a2: => F[A2], a3: => F[A3], a4: => F[A4])(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z] =
    super[Alt].xproduct4(a1, a2, a3, a4)(f, g)

  override def xcoproduct1[Z, A1](a1: => F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    super[Alt].xcoproduct1(a1)(f, g)
  override def xcoproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: A1 \/ A2 => Z, g: Z => A1 \/ A2): F[Z] =
    super[Alt].xcoproduct2(a1, a2)(f, g)
  override def xcoproduct3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(f: A1 \/ (A2 \/ A3) => Z, g: Z => A1 \/ (A2 \/ A3)): F[Z] =
    super[Alt].xcoproduct3(a1, a2, a3)(f, g)
  override def xcoproduct4[Z, A1, A2, A3, A4](a1: => F[A1], a2: => F[A2], a3: => F[A3], a4: => F[A4])(f: A1 \/ (A2 \/ (A3 \/ A4)) => Z, g: Z => A1 \/ (A2 \/ (A3 \/ A4))): F[Z] =
    super[Alt].xcoproduct4(a1, a2, a3, a4)(f, g)

  ////
}
