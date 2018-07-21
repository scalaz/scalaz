package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/**
 * Coproduct analogue of Divide
 *
 * https://hackage.haskell.org/package/contravariant-1.4.1/docs/Data-Functor-Contravariant-Divisible.html#t:Decidable
 */
////
trait Decidable[F[_]] extends Divisible[F] with InvariantAlt[F] { self =>
  ////

  // backport from 7.3's Divisible
  override def contramap[A, B](fa: F[A])(f: B => A): F[B] =
    divide2(conquer[Unit], fa)(c => ((), f(c)))

  final def choose[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => A1 \/ A2): F[Z] = choose2(a1, a2)(f)

  def choose1[Z, A1](a1: =>F[A1])(f: Z => A1): F[Z] = contramap(a1)(f)
  def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => A1 \/ A2): F[Z]
  def choose3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: Z => A1 \/ (A2 \/ A3)
  ): F[Z] = {
    val a23: F[A2 \/ A3] = choose2(a2, a3)(identity)
    choose2(a1, a23)(f)
  }
  def choose4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z] = {
    val a234: F[A2 \/ (A3 \/ A4)] = choose3(a2, a3, a4)(identity)
    choose2(a1, a234)(f)
  }
  // ... chooseN

  final def choosing2[Z, A1, A2](
    f: Z => A1 \/ A2
  )(implicit fa1: F[A1], fa2: F[A2]): F[Z] =
    choose2(fa1, fa2)(f)
  final def choosing3[Z, A1, A2, A3](
    f: Z => A1 \/ (A2 \/ A3)
  )(implicit fa1: F[A1], fa2: F[A2], fa3: F[A3]): F[Z] =
    choose3(fa1, fa2, fa3)(f)
  final def choosing4[Z, A1, A2, A3, A4](
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  )(implicit fa1: F[A1], fa2: F[A2], fa3: F[A3], fa4: F[A4]): F[Z] =
    choose4(fa1, fa2, fa3, fa4)(f)
  // ... choosingX

  override def xcoproduct1[Z, A1](a1: =>F[A1])(
    f: A1 => Z,
    g: Z => A1
  ): F[Z] = choose1(a1)(g)
  override def xcoproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1 \/ A2) => Z,
    g: Z => (A1 \/ A2)
  ): F[Z] = choose2(a1, a2)(g)
  override def xcoproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1 \/ (A2 \/ A3)) => Z,
    g: Z => (A1 \/ (A2 \/ A3))
  ): F[Z] = choose3(a1, a2, a3)(g)
  override def xcoproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
    g: Z => (A1 \/ (A2 \/ (A3 \/ A4)))
  ): F[Z] = choose4(a1, a2, a3, a4)(g)

  // from 7.3's Divide
  override def xproduct0[Z](z: =>Z): F[Z] = conquer
  override def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    xmap(a1, f, g)
  override def xproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z,
    g: Z => (A1, A2)
  ): F[Z] = divide2(a1, a2)(g)
  override def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = divide3(a1, a2, a3)(g)
  override def xproduct4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  ): F[Z] = divide4(a1, a2, a3, a4)(g)

  trait DecidableLaw extends DivisibleLaw {
    // distribution law blocked on https://github.com/ekmett/contravariant/issues/53

    // TODO: translate this
    // choose f (choose g m n) o = divide f' m (divide id n o) where
    //   f' bcd = either (either id (Right . Left) . g) (Right . Right) . f
  }
  def decidableLaw = new DecidableLaw {}

  ////
  val decidableSyntax = new scalaz.syntax.DecidableSyntax[F] { def F = Decidable.this }
}

object Decidable {
  @inline def apply[F[_]](implicit F: Decidable[F]): Decidable[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Decidable[G]): Decidable[F] =
    new IsomorphismDecidable[F, G] {
      override def G: Decidable[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}
