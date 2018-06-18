package scalaz

////
/**
 * [[scalaz.Applicative]] combined with [[scalaz.PlusEmpty]].
 */
////
trait ApplicativePlus[F[_]] extends Applicative[F] with PlusEmpty[F] with Derives[F] { self =>
  ////

  // FIXME
  def coapply1[Z, A1](a1: =>F[A1])(f: A1 => Z): F[Z] = ???
  def coapply2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: A1 \/ A2 => Z): F[Z] = ???
  def coapply3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: A1 \/ (A2 \/ A3) => Z
  ): F[Z] = coapply2(a1, either2(a2, a3))(f)
  def coapply4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3],a4: =>F[A4])(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  ): F[Z] = coapply2(a1, either2(a2, either2(a3, a4)))(f)
  // ... coapplyN

  // equivalent of tupleN
  def either2[A1, A2](a1: =>F[A1], a2: =>F[A2]): F[A1 \/ A2] =
    coapply2(a1, a2)(identity)
  // ... eitherN

  final def coapplying1[Z, A1](
    f: A1 => Z
  )(implicit a1: F[A1]): F[Z] =
    coapply1(a1)(f)
  final def coapplying2[Z, A1, A2](
    f: A1 \/ A2 => Z
  )(implicit a1: F[A1], a2: F[A2]): F[Z] =
    coapply2(a1, a2)(f)
  final def coapplying3[Z, A1, A2, A3](
    f: A1 \/ (A2 \/ A3) => Z
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3]): F[Z] =
    coapply3(a1, a2, a3)(f)
  final def coapplying4[Z, A1, A2, A3, A4](
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[Z] =
    coapply4(a1, a2, a3, a4)(f)
  // ... coapplyingX

  override def xproduct0[Z](z: =>Z): F[Z] = pure(z)
  override def xproduct1[Z, A1](a1: F[A1])(f: A1 => Z, g: Z => A1): F[Z] = xmap(a1, f, g)
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
  ): F[Z] = coapply1(a1)(f)
  override def xcoproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1 \/ A2) => Z,
    g: Z => (A1 \/ A2)
  ): F[Z] = coapply2(a1, a2)(f)
  override def xcoproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1 \/ (A2 \/ A3)) => Z,
    g: Z => (A1 \/ (A2 \/ A3))
  ): F[Z] = coapply3(a1, a2, a3)(f)
  override def xcoproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
    g: Z => (A1 \/ (A2 \/ (A3 \/ A4)))
  ): F[Z] = coapply4(a1, a2, a3, a4)(f)

  /**The composition of ApplicativePlus `F` and Applicative `G`, `[x]F[G[x]]`, is a ApplicativePlus */
  override def compose[G[_]](implicit G0: Applicative[G]): ApplicativePlus[λ[α => F[G[α]]]] =
    new CompositionApplicativePlus[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /**The product of ApplicativePlus `F` and `G`, `[x](F[x], G[x]])`, is a ApplicativePlus */
  def product[G[_]](implicit G0: ApplicativePlus[G]): ApplicativePlus[λ[α => (F[α], G[α])]] =
    new ProductApplicativePlus[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  private[this] class Mutual[A](a: F[A]) {
    lazy val y: Free.Trampoline[F[IList[A]]] = z map (plus(_, point(INil())))
    lazy val z: Free.Trampoline[F[IList[A]]] = y map (apply2(a, _)(_ :: _))
  }

  /** `empty` or a non-empty list of results acquired by repeating `a`. */
  def some[A](a: F[A]): F[IList[A]] = new Mutual(a).z.run

  /** A list of results acquired by repeating `a`.  Never `empty`;
    * initial failure is an empty list instead.
    */
  def many[A](a: F[A]): F[IList[A]] = new Mutual(a).y.run

  ////
  val applicativePlusSyntax = new scalaz.syntax.ApplicativePlusSyntax[F] { def F = ApplicativePlus.this }
}

object ApplicativePlus {
  @inline def apply[F[_]](implicit F: ApplicativePlus[F]): ApplicativePlus[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: ApplicativePlus[G]): ApplicativePlus[F] =
    new IsomorphismApplicativePlus[F, G] {
      override def G: ApplicativePlus[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismApplicativePlus[F[_], G[_]] extends ApplicativePlus[F] with IsomorphismApplicative[F, G] with IsomorphismPlusEmpty[F, G] with IsomorphismDerives[F, G]{
  implicit def G: ApplicativePlus[G]
  ////

  ////
}
