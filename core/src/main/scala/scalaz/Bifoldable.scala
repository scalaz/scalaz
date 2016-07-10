package scalaz

////
/**
 * A type giving rise to two unrelated [[scalaz.Foldable]]s.
 */
////
trait Bifoldable[F[_, _]]  { self =>
  ////

  /** Accumulate `A`s and `B`s */
  def bifoldMap[A,B,M](fa: F[A, B])(f: A => M)(g: B => M)(implicit F: Monoid[M]): M

  /** Accumulate to `C` starting at the "right".  `f` and `g` may be
    * interleaved.
    */
  def bifoldRight[A,B,C](fa: F[A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C

  // derived functions

  /** `bifoldRight`, but defined to run in the opposite direction. */
  def bifoldLeft[A,B,C](fa: F[A, B], z: C)(f: (C, A) => C)(g: (C, B) => C): C = {
    import Dual._, Endo._, syntax.std.all._
    Tag.unwrap(bifoldMap(fa)((a: A) => Dual(Endo.endo(f.flip.curried(a))))((b: B) => Dual(Endo.endo(g.flip.curried(b))))(dualMonoid[Endo[C]])) apply z
  }

  /**The composition of Bifoldables `F` and `G`, `[x,y]F[G[x,y],G[x,y]]`, is a Bifoldable */
  def compose[G[_, _]](implicit G0: Bifoldable[G]): Bifoldable[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new CompositionBifoldable[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /**The product of Bifoldables `F` and `G`, `[x,y](F[x,y], G[x,y])`, is a Bifoldable */
  def product[G[_, _]](implicit G0: Bifoldable[G]): Bifoldable[λ[(α, β) => (F[α, β], G[α, β])]] =
    new ProductBifoldable[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  // derived functions
  def bifoldMap1[A,B,M](fa: F[A,B])(f: A => M)(g: B => M)(implicit F: Semigroup[M]): Option[M] = {
    import std.option._
    bifoldMap(fa)(a => some(f(a)))(b => some(g(b)))
  }

  /**Curried version of `bifoldRight` */
  final def bifoldR[A, B, C](fa: F[A, B], z: => C)(f: A => (=> C) => C)(g: B => (=> C) => C): C =
    bifoldRight(fa, z)(Function.uncurried(f))(Function.uncurried(g))

  /**Curried version of `bifoldLeft` */
  final def bifoldL[A, B, C](fa: F[A, B], z: C)(f: C => A => C)(g: C => B => C): C =
    bifoldLeft(fa, z)(Function.uncurried(f))(Function.uncurried(g))

  /** Extract the Foldable on the first parameter. */
  def leftFoldable[X]: Foldable[F[?, X]] =
    new LeftFoldable[F, X] {val F = self}

  /** Extract the Foldable on the second parameter. */
  def rightFoldable[X]: Foldable[F[X, ?]] =
    new RightFoldable[F, X] {val F = self}

  /** Unify the foldable over both params. */
  def uFoldable: Foldable[λ[α => F[α, α]]] =
    new UFoldable[F] {val F = self}

  /** Embed one Foldable at each side of this Bifoldable */
  def embed[G[_],H[_]](implicit G0: Foldable[G], H0: Foldable[H]): Bifoldable[λ[(α, β) => F[G[α],H[β]]]] =
    new CompositionBifoldableFoldables[F,G,H] {
      def F = self
      def G = G0
      def H = H0
    }

  /** Embed one Foldable to the left of this Bifoldable .*/
  def embedLeft[G[_]](implicit G0: Foldable[G]): Bifoldable[λ[(α, β) => F[G[α],β]]] =
    embed[G,Id.Id]

  /** Embed one Foldable to the right of this Bifoldable .*/
  def embedRight[H[_]](implicit H0: Foldable[H]): Bifoldable[λ[(α, β) => F[α,H[β]]]] =
    embed[Id.Id,H]

  trait BifoldableLaw {
    import std.vector._

    def leftFMConsistent[A: Equal, B: Equal](fa: F[A, B]): Boolean =
      Equal[Vector[B \/ A]].equal(bifoldMap[A, B, Vector[B \/ A]](fa)(a => Vector(\/-(a)))(b => Vector(-\/(b))),
                                  bifoldLeft(fa, Vector.empty[B \/ A])(_ :+ \/-(_))(_ :+ -\/(_)))

    def rightFMConsistent[A: Equal, B: Equal](fa: F[A, B]): Boolean =
      Equal[Vector[B \/ A]].equal(bifoldMap[A, B, Vector[B \/ A]](fa)(a => Vector(\/-(a)))(b => Vector(-\/(b))),
                                  bifoldRight(fa, Vector.empty[B \/ A])(\/-(_) +: _)(-\/(_) +: _))
  }

  def bifoldableLaw = new BifoldableLaw {}
  ////
  val bifoldableSyntax = new scalaz.syntax.BifoldableSyntax[F] { def F = Bifoldable.this }
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
   * Template trait to define `Bifoldable` in terms of `bifoldR`
   */
  trait FromBifoldr[F[_, _]] extends Bifoldable[F] {
    override def bifoldMap[A, B, M](fa: F[A, B])(f: A => M)(g: B => M)(implicit F: Monoid[M]) =
      bifoldR(fa, F.zero)(x => y => F.append(f(x),  y))(x => y => F.append(g(x),  y))
  }

  ////
}
