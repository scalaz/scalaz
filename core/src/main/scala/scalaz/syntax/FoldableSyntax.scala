package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Foldable` */
final class FoldableOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Foldable[F]) extends Ops[F[A]] {
  ////
  import collection.generic.CanBuildFrom
  import Leibniz.===
  import Liskov.<~<

  final def foldMap[B: Monoid](f: A => B = (a: A) => a): B = F.foldMap(self)(f)
  final def foldMap1Opt[B: Semigroup](f: A => B = (a: A) => a): Option[B] = F.foldMap1Opt(self)(f)
  final def foldRight[B](z: => B)(f: (A, => B) => B): B = F.foldRight(self, z)(f)
  final def foldMapRight1Opt[B](z: A => B)(f: (A, => B) => B): Option[B] = F.foldMapRight1Opt(self)(z)(f)
  final def foldRight1Opt(f: (A, => A) => A): Option[A] = F.foldRight1Opt(self)(f)
  final def foldLeft[B](z: B)(f: (B, A) => B): B = F.foldLeft(self, z)(f)
  final def foldMapLeft1Opt[B](z: A => B)(f: (B, A) => B): Option[B] = F.foldMapLeft1Opt(self)(z)(f)
  final def foldLeft1Opt(f: (A, A) => A): Option[A] = F.foldLeft1Opt(self)(f)
  final def foldRightM[G[_], B](z: => B)(f: (A, => B) => G[B])(implicit M: Monad[G]): G[B] = F.foldRightM(self, z)(f)
  final def foldLeftM[G[_], B](z: B)(f: (B, A) => G[B])(implicit M: Monad[G]): G[B] = F.foldLeftM(self, z)(f)
  final def foldMapM[G[_] : Monad, B : Monoid](f: A => G[B]): G[B] = F.foldMapM(self)(f)
  final def findMapM[G[_] : Monad, B](f: A => G[Option[B]]): G[Option[B]] = F.findMapM(self)(f)
  final def findLeft(f: A => Boolean): Option[A] = F.findLeft(self)(f)
  final def findRight(f: A => Boolean): Option[A] = F.findRight(self)(f)
  final def fold(implicit A: Monoid[A]): A = F.fold(self)(A)
  final def foldr[B](z: => B)(f: A => (=> B) => B): B = F.foldr(self, z)(f)
  final def foldr1Opt(f: A => (=> A) => A): Option[A] = F.foldr1Opt(self)(f)
  final def foldl[B](z: B)(f: B => A => B): B = F.foldl(self, z)(f)
  final def foldl1Opt(f: A => A => A): Option[A] = F.foldl1Opt(self)(f)
  final def foldrM[G[_], B](z: => B)(f: A => ( => B) => G[B])(implicit M: Monad[G]): G[B] = F.foldrM(self, z)(f)
  final def foldlM[G[_], B](z: B)(f: B => A => G[B])(implicit M: Monad[G]): G[B] = F.foldlM(self, z)(f)
  final def length: Int = F.length(self)
  final def index(n: Int): Option[A] = F.index(self, n)
  final def indexOr(default: => A, n: Int): A = F.indexOr(self, default, n)
  final def sumr(implicit A: Monoid[A]): A = F.sumr(self)
  final def sumr1Opt(implicit A: Semigroup[A]): Option[A] = F.sumr1Opt(self)
  final def suml(implicit A: Monoid[A]): A = F.suml(self)
  final def suml1Opt(implicit A: Semigroup[A]): Option[A] = F.suml1Opt(self)
  final def toList: List[A] = F.toList(self)
  final def toVector: Vector[A] = F.toVector(self)
  final def toSet: Set[A] = F.toSet(self)
  final def toStream: Stream[A] = F.toStream(self)
  final def toIList: IList[A] = F.toIList(self)
  final def toEphemeralStream: EphemeralStream[A] = F.toEphemeralStream(self)
  final def to[G[_]](implicit c: CanBuildFrom[Nothing, A, G[A]]) = F.to[A, G](self)
  final def all(p: A => Boolean): Boolean = F.all(self)(p)
  final def ∀(p: A => Boolean): Boolean = F.all(self)(p)
  final def allM[G[_]: Monad](p: A => G[Boolean]): G[Boolean] = F.allM(self)(p)
  final def anyM[G[_]: Monad](p: A => G[Boolean]): G[Boolean] = F.anyM(self)(p)
  final def any(p: A => Boolean): Boolean = F.any(self)(p)
  final def ∃(p: A => Boolean): Boolean = F.any(self)(p)
  final def filterLength(p: A => Boolean): Int = F.foldLeft(self, 0)((b, a) => (if (p(a)) 1 else 0) + b)
  final def count: Int = F.count(self)
  final def maximum(implicit A: Order[A]): Option[A] = F.maximum(self)
  final def maximumOf[B: Order](f: A => B): Option[B] = F.maximumOf(self)(f)
  final def maximumBy[B: Order](f: A => B): Option[A] = F.maximumBy(self)(f)
  final def minimum(implicit A: Order[A]): Option[A] = F.minimum(self)
  final def minimumOf[B: Order](f: A => B): Option[B] = F.minimumOf(self)(f)
  final def minimumBy[B: Order](f: A => B): Option[A] = F.minimumBy(self)(f)
  final def distinct(implicit A: Order[A]): IList[A] = F.distinct(self)
  final def distinctE(implicit A: Equal[A]): IList[A] = F.distinctE(self)
  final def longDigits(implicit d: A <:< Digit): Long = F.longDigits(self)
  final def empty: Boolean = F.empty(self)
  final def element(a: A)(implicit A: Equal[A]): Boolean = F.element(self, a)
  final def splitWith(p: A => Boolean): List[NonEmptyList[A]] = F.splitWith(self)(p)
  final def splitBy[B: Equal](f: A => B): IList[(B, NonEmptyList[A])] =
    F.foldRight(self, IList.empty[(B, NonEmptyList[A])]){ (a, bas) =>
      val fa = f(a)
      bas match {
        case INil() =>
          IList.single((fa, NonEmptyList.nel(a, IList.empty)))
        case ICons((b, as), tail) =>
          if (Equal[B].equal(fa, b)) ICons((b, a <:: as), tail)
          else ICons((fa, NonEmptyList.nel(a, IList.empty)), bas)
      }
    }
  final def splitByRelation(r: (A, A) => Boolean): IList[NonEmptyList[A]] =
    F.foldRight(self, IList.empty[NonEmptyList[A]]){ (a, neas) =>
      neas match {
        case INil() =>
          IList.single(NonEmptyList.nel(a, IList.empty))
        case ICons(nea, tail) =>
          if (r(a, nea.head)) ICons(a <:: nea, tail)
          else ICons(NonEmptyList.nel(a, IList.empty), neas)
      }
    }
  final def selectSplit(p: A => Boolean): List[NonEmptyList[A]] = F.selectSplit(self)(p)
  final def collapse[X[_]](implicit A: ApplicativePlus[X]): X[A] = F.collapse(self)
  final def concatenate(implicit A: Monoid[A]): A = F.fold(self)
  final def intercalate(a: A)(implicit A: Monoid[A]): A = F.intercalate(self, a)
  final def traverse_[M[_]:Applicative](f: A => M[Unit]): M[Unit] = F.traverse_(self)(f)
  final def traverseU_[GB](f: A => GB)(implicit G: Unapply[Applicative, GB]): G.M[Unit] =
    F.traverseU_[A, GB](self)(f)(G)
  final def traverseS_[S, B](f: A => State[S, B]): State[S, Unit] = F.traverseS_(self)(f)
  final def sequence_[G[_], B](implicit ev: A === G[B], G: Applicative[G]): G[Unit] = F.sequence_(ev.subst[F](self))(G)
  final def sequenceS_[S, B](implicit ev: A === State[S,B]): State[S,Unit] = F.sequenceS_(ev.subst[F](self))
  def sequenceF_[M[_],B](implicit ev: F[A] <~< F[Free[M,B]]): Free[M, Unit] = F.sequenceF_(ev(self))
  final def msuml[G[_], B](implicit ev: A === G[B], G: PlusEmpty[G]): G[B] = F.msuml(ev.subst[F](self))
  final def msumlU(implicit G: Unapply[PlusEmpty, A]): G.M[G.A] = F.msuml[G.M, G.A](G.leibniz.subst[F](self))(G.TC)
  ////
}

sealed trait ToFoldableOps0 {
  implicit def ToFoldableOpsUnapply[FA](v: FA)(implicit F0: Unapply[Foldable, FA]) =
    new FoldableOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToFoldableOps extends ToFoldableOps0 {
  implicit def ToFoldableOps[F[_],A](v: F[A])(implicit F0: Foldable[F]) =
    new FoldableOps[F,A](v)

  ////

  ////
}

trait FoldableSyntax[F[_]]  {
  implicit def ToFoldableOps[A](v: F[A]): FoldableOps[F, A] = new FoldableOps[F,A](v)(FoldableSyntax.this.F)

  def F: Foldable[F]
  ////

  ////
}
