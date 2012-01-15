package scalaz

////
/**
 *
 */
////
trait Foldable[F[_]]  { self =>
  ////
  /** Map each element of the structure to a [[scalaz.Monoid]], and combine the results. */
  def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B

  /**Right-associative fold of a structure. */
  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

//  /**Right-associative fold of a structure. */
//  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B =
//    foldMap(fa)((a: A) => (Endo.endo(f.curried(a)(_: B)))) apply z

  /**Left-associative fold of a structure. */
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B = {
    import Dual._, Endo._, syntax.std.allV._
    foldMap(fa)((a: A) => Dual(Endo.endo(f.flip.curried(a))))(dualMonoid) apply (z)
  }

  // derived functions
  def foldMap1[A,B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): Option[B] = {
    import std.option._
    foldMap(fa)(a => some(f(a)))
  }

  /** Combine the elements of a structure using a monoid. */
  def fold[M: Monoid](t: F[M]): M = foldMap[M, M](t)(x => x)

  /**Curried version of `foldRight` */
  final def foldR[A, B](fa: F[A], z: => B)(f: A => (=> B) => B): B = foldRight(fa, z)((a, b) => f(a)(b))

  /**Curred version of `foldLeft` */
  final def foldL[A, B](fa: F[A], z: B)(f: B => A => B) = foldLeft(fa, z)((b, a) => f(b)(a))

  def foldMapIdentity[A,B](fa: F[A])(implicit F: Monoid[A]): A = foldMap(fa)(a => a)
  def foldR1[A](fa: F[A])(f: (A => (=> A) => A)): Option[A] = foldR(fa, None: Option[A])(a => o => o.map(f(a)(_)) orElse Some(a))
  def foldL1[A](fa: F[A])(f: (A => (=> A) => A)): Option[A] = foldL(fa, None: Option[A])(o => a => o.map(f(a)(_)) orElse Some(a))
  def toList[A](fa: F[A]): List[A] = foldLeft(fa, scala.List[A]())((t, h) => h :: t).reverse
  def toIndexedSeq[A](fa: F[A]): IndexedSeq[A] = foldLeft(fa, IndexedSeq[A]())(_ :+ _)
  def toSet[A](fa: F[A]): Set[A] = foldLeft(fa, Set[A]())(_ + _)
  def toStream[A](fa: F[A]): Stream[A] = foldRight[A, Stream[A]](fa, Stream.empty)(Stream.cons(_, _))

  def all[A](fa: F[A])(p: A => Boolean): Boolean = foldRight(fa, true)(p(_) && _)
  def any[A](fa: F[A])(p: A => Boolean): Boolean = foldRight(fa, false)(p(_) || _)
  def count[A](fa: F[A]): Int = foldLeft(fa, 0)((b, _) => b + 1)
  import Ordering.{GT, LT}
  def maximum[A: Order](fa: F[A]): Option[A] = foldL1(fa)(x => y => if (Order[A].order(x, y) == GT) x else y)
  def minimum[A: Order](fa: F[A]): Option[A] = foldL1(fa)(x => y => if (Order[A].order(x, y) == LT) x else y)
  def longDigits[A](fa: F[A])(implicit d: A <:< Digit): Long = foldLeft(fa, 0L)((n, a) => n * 10L + (a: Digit))
  def empty[A](fa: F[A]): Boolean = all(fa)(_ => false)
  def element[A: Equal](fa: F[A], a: A): Boolean = any(fa)(Equal[A].equal(a, _))

  /**
   * Splits the elements into groups that alternatively satisfy and don't satisfy the predicate p.
   */
  def splitWith[A](fa: F[A])(p: A => Boolean): List[List[A]] =
    foldRight(fa, (List[List[A]](), None : Option[Boolean]))((a, b) => {
      val pa = p(a)
      (b match {
        case (_, None) => List(List(a))
        case (x, Some(q)) => if (pa == q) (a :: x.head) :: x.tail else List(a) :: x
      }, Some(pa))
    })._1


  /**
   * Selects groups of elements that satisfy p and discards others.
   */
  def selectSplit[A](fa: F[A])(p: A => Boolean): List[List[A]] =
    foldRight(fa, (List[List[A]](), false))((a, xb) => xb match {
      case (x, b) => {
        val pa = p(a)
        (if (pa)
          if (b)
            (a :: x.head) :: x.tail else
            List(a) :: x
        else x, pa)
      }
    })._1

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
    override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B) =
      foldMap(fa)((a: A) => (Endo.endo(f(a, _: B)))) apply z
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

