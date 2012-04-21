package scalaz

////
/**
 *
 */
////
trait Bifoldable[F[_, _]]  { self =>
  ////

  def bifoldMap[A,B,M](fa: F[A, B])(f: A => M)(g: B => M)(implicit F: Monoid[M]): M

  def bifoldRight[A,B,C](fa: F[A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C

  // derived functions

  def bifoldLeft[A,B,C](fa: F[A, B], z: C)(f: (C, A) => C)(g: (C, B) => C): C = {
    import Dual._, Endo._, syntax.std.allV._
    bifoldMap(fa)((a: A) => Dual(Endo.endo(f.flip.curried(a))))((b: B) => Dual(Endo.endo(g.flip.curried(b))))(dualMonoid[Endo[C]]) apply z
  }

  /**The composition of Bifoldables `F` and `G`, `[x,y]F[G[x,y],G[x,y]]`, is a Bifoldable */
  def compose[G[_, _]](implicit G0: Bifoldable[G]): Bifoldable[({type λ[α, β]=F[G[α, β], G[α, β]]})#λ] = new CompositionBifoldable[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Bifoldables `F` and `G`, `[x,y]F[G[x,y],G[x,y]]`, is a Bifoldable */
  def product[G[_, _]](implicit G0: Bifoldable[G]): Bifoldable[({type λ[α, β]=(F[α, β], G[α, β])})#λ] = new ProductBifoldable[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  // derived functions
  def bifoldMap1[A,B,M](fa: F[A,B])(f: A => M)(g: B => M)(implicit F: Semigroup[M]): Option[M] = {
    import std.option._
    bifoldMap(fa)(a => some(f(a)))(b => some(g(b)))
  }

  /**Curried version of `foldRight` */
  final def bifoldR[A, B, C](fa: F[A, B], z: => C)(f: A => (=> C) => C)(g: B => (=> C) => C): C =
    bifoldRight(fa, z)(Function.uncurried(f))(Function.uncurried(g))

  /**Curred version of `foldLeft` */
  final def bifoldL[A, B, C](fa: F[A, B], z: C)(f: C => A => C)(g: C => B => C): C =
    bifoldLeft(fa, z)(Function.uncurried(f))(Function.uncurried(g))

  ////
  val bifoldableSyntax = new scalaz.syntax.BifoldableSyntax[F] {}
}

object Bifoldable {
  @inline def apply[F[_, _]](implicit F: Bifoldable[F]): Bifoldable[F] = F

  ////
  /**
   * Template trait to define `Bifoldable` in terms of `bifoldMap`.
   */
  trait FromBifoldMap[F[_, _]] extends Bifoldable[F] {
    override def bifoldRight[A,B,C](fa: F[A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C) =
      bifoldMap(fa)((a: A) => (Endo.endo(f(a, _: C))))((b: B) => (Endo.endo(g(b, _: C)))) apply z
  }

  /**
   * Template trait to define `Foldable` in terms of `foldr`
   */
  trait FromBifoldr[F[_, _]] extends Bifoldable[F] {
    override def bifoldMap[A, B, M](fa: F[A, B])(f: A => M)(g: B => M)(implicit F: Monoid[M]) =
      bifoldR(fa, F.zero)(x => y => F.append(f(x),  y))(x => y => F.append(g(x),  y))
  }

  ////
}

