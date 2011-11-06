package scalaz

trait Foldable[F[_]]  { self =>
  ////
  /** Map each element of the structure to a monoid, and combine the results. */
  def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B

  /**Right-associative fold of a structure. */
  // TODO uncurry `f`.
  def foldR[A, B](fa: F[A], z: B)(f: A => (=> B) => B): B

  /**Left-associative fold of a structure. */
  def foldL[A, B](fa: F[A], z: B)(f: (B, A) => B) = {
    import Dual._, Endo._, syntax.std.allV._
    foldMap(fa)((a: A) => Dual(Endo.endo(f.flip.curried(a))))(dualMonoid) apply (z)
  }

  // derived functions

  /** Combine the elements of a structure using a monoid. */
  def fold[M: Monoid](t: F[M]): M = foldMap[M, M](t)(x => x)

  def foldMapIdentity[A,B](fa: F[A])(implicit F: Monoid[A]): A = foldMap(fa)(a => a)
  def foldR1[A](fa: F[A])(f: (A => (=> A) => A)): Option[A] = foldR(fa, None: Option[A])(a => o => o.map(f(a)(_)) orElse Some(a))
  def toList[A](fa: F[A]): List[A] = foldL(fa, scala.List[A]())((t, h) => h :: t).reverse
  def toIndexedSeq[A](fa: F[A]): IndexedSeq[A] = foldL(fa, IndexedSeq[A]())(_ :+ _)
  def toSet[A](fa: F[A]): Set[A] = foldL(fa, Set[A]())(_ + _)

  // TODO max/min/element/any/all etc.

  ////
  val foldableSyntax = new scalaz.syntax.FoldableSyntax[F] {}
}

object Foldable {
  @inline def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F

  ////
  /**
   * Template trait to define `Foldable` in terms of `foldMap`.
   *
   * Example:
   * {{{
   * new Foldable[Option] with Foldable.FromFoldMap[Option] {
   *   def foldMap[A, B](fa: Option[A])(f: (A) => B)(implicit F: Monoid[B]) = fa match {
   *     case Some(a) => f(a)
   *     case None    => F.zero
   *   }
   * }
   * }}}
   */
  trait FromFoldMap[F[_]] extends Foldable[F] {
    override def foldR[A, B](fa: F[A], z: B)(f: (A) => (=> B) => B) =
      foldMap(fa)((a: A) => (Endo.endo(f(a)(_: B)))) apply z
  }

  /**
   * Template trait to define `Foldable` in terms of `foldr`
   *
   * Example:
   * {{{
   * new Foldable[Option] with Foldable.FromFoldr[Option] {
   *   def foldR[A, B](fa: Option[A], z: B)(f: (A) => (=> B) => B) = fa match {
   *     case Some(a) => f(a)(z)
   *     case None => z
   *   }
   * }
   * }}}
   */
  trait FromFoldr[F[_]] extends Foldable[F] {
    override def foldMap[A, B](fa: F[A])(f: (A) => B)(implicit F: Monoid[B]) =
        foldR[A, B](fa, F.zero)( x => y => F.append(f(x),  y))
  }

  ////
}

