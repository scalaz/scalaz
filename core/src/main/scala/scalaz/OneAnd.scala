package scalaz

import scalaz.Isomorphism.{<~>, IsoFunctorTemplate}
import scalaz.std.option.{optionMonoid, none, some}
import scalaz.Ordering.orderingInstance

/** A generalization of `NonEmptyList` to non-`List` things.  For
  * example, `OneAnd[Vector, A]` is a non-empty `Vector` of `A`.
  *
  * Only `head` and `tail` are provided as direct methods, because
  * there's little you can do with a `OneAnd` without knowing a bit
  * about `F`.  So useful functions are provided in the form of
  * typeclass instances on `OneAnd`'s companion; in combination with
  * syntax extensions provided by `scalaz.syntax`, `OneAnd` has a
  * large possible set of methods available by importing.  For
  * example, `Applicative` on this requires `ApplicativePlus[F]`, and
  * `Traverse1` on this requires `Traverse[F]`.  See the companion
  * documentation for a complete list of instances.
  *
  * Additionally, unlike `λ[α => (α, F[α])]`, the behavior of typeclass
  * operations on `OneAnd` should be preserved across the natural
  * transformation to [[scalaz.NonEmptyList]] where one exists.  That
  * is it should "be like" a nonempty structure.  For example,
  * `ap`-ing two `OneAnd[Vector, *]`s or `NonEmptyList`s of lengths
  * *a* and *b* yields a sequence of same type of length *a*×*b*, but
  * two `λ[α => (α, Vector[α])]`s of length *c* and *d* `ap` to one of
  * length 1+(*c*–1)×(*d*–1).  For another example, `point` of the
  * former two yield a one-element sequence, but `point` of the latter
  * yields a two-element sequence.
  *
  * @since 7.0.3
  */
final case class OneAnd[F[_], A](head: A, tail: F[A])

private sealed trait OneAndFunctor[F[_]] extends Functor[OneAnd[F, *]] {
  def F: Functor[F]

  override def map[A, B](fa: OneAnd[F, A])(f: A => B): OneAnd[F, B] =
    OneAnd(f(fa.head), F.map(fa.tail)(f))
}

private sealed trait OneAndApply[F[_]] extends Apply[OneAnd[F, *]] with OneAndFunctor[F] {
  def F: Applicative[F]
  def G: Plus[F]

  override def ap[A, B](fa: => OneAnd[F, A])(f: => OneAnd[F, A => B]): OneAnd[F, B] = {
    val OneAnd(hf, tf) = f
    val OneAnd(ha, ta) = fa
    OneAnd(hf(ha), G.plus(F.map(ta)(hf),
                          F.ap(G.plus(F.point(ha), ta))(tf)))
  }
}

private sealed trait OneAndAlign[F[_]] extends Align[OneAnd[F, *]] with OneAndFunctor[F] {
  def F: Align[F]

  override def alignWith[A, B, C](f: A \&/ B => C) = {
    case (OneAnd(ha, ta), OneAnd(hb, tb)) =>
      OneAnd(f(\&/.Both(ha, hb)), F.alignWith(f)(ta, tb))
  }
}

private sealed trait OneAndApplicative[F[_]] extends Applicative[OneAnd[F, *]] with OneAndApply[F] {
  def F: ApplicativePlus[F]

  def point[A](a: => A): OneAnd[F, A] = OneAnd(a, F.empty)
}

private sealed trait OneAndBind[F[_]] extends Bind[OneAnd[F, *]] with OneAndApply[F] {
  def F: Monad[F]
  def G: Plus[F]

  def bind[A, B](fa: OneAnd[F, A])(f: A => OneAnd[F, B]): OneAnd[F, B] = OneAnd(
    f(fa.head).head,
    G.plus(
      f(fa.head).tail,
      F.bind(fa.tail){ a =>
        val x = f(a)
        G.plus(F.point(x.head), x.tail)
      }
    )
  )
}

private sealed trait OneAndPlus[F[_]] extends Plus[OneAnd[F, *]] {
  def F: Applicative[F]
  def G: Plus[F]

  def plus[A](a: OneAnd[F, A], b: => OneAnd[F, A]): OneAnd[F, A] =
    OneAnd(a.head, G.plus(G.plus(a.tail, F.point(b.head)), b.tail))
}

private sealed trait OneAndMonad[F[_]] extends Monad[OneAnd[F, *]] with OneAndBind[F] with OneAndApplicative[F] {
  def F: MonadPlus[F]
  def G: MonadPlus[F] = F
}

private sealed trait OneAndFoldable[F[_]] extends Foldable1[OneAnd[F, *]] {
  def F: Foldable[F]

  override def findLeft[A](fa: OneAnd[F, A])(f: A => Boolean) =
    if(f(fa.head)) Some(fa.head) else F.findLeft(fa.tail)(f)

  override def findRight[A](fa: OneAnd[F, A])(f: A => Boolean) =
    F.findRight(fa.tail)(f) match {
      case a @ Some(_) =>
        a
      case None =>
        if(f(fa.head)) Some(fa.head) else None
    }

  override def foldMap1[A, B: Semigroup](fa: OneAnd[F, A])(f: A => B) =
    foldMap(fa)(a => some(f(a))) getOrElse f(fa.head)

  override def foldMapRight1[A, B](fa: OneAnd[F, A])(z: A => B)(f: (A, => B) => B) =
    (F.foldRight(fa.tail, none[B])((a, ob) => ob map (f(a, _)) orElse some(z(a)))
       map (f(fa.head, _)) getOrElse z(fa.head))

  override def foldMapLeft1[A, B](fa: OneAnd[F, A])(z: A => B)(f: (B, A) => B) =
    F.foldLeft(fa.tail, z(fa.head))(f)

  override def foldMap[A, B](fa: OneAnd[F, A])(f: A => B)(implicit M: Monoid[B]) =
    M.append(f(fa.head), F.foldMap(fa.tail)(f))

  override def foldRight[A, B](fa: OneAnd[F, A], z: => B)(f: (A, => B) => B) =
    f(fa.head, F.foldRight(fa.tail, z)(f))

  override def foldLeft[A, B](fa: OneAnd[F, A], z: B)(f: (B, A) => B) =
    F.foldLeft(fa.tail, f(z, fa.head))(f)

  override def traverseS_[S,A,B](fa: OneAnd[F, A])(f: A => State[S,B]) =
    State{(s: S) => F.traverseS_(fa.tail)(f)(f(fa.head)(s)._1)}

  override def length[A](fa: OneAnd[F, A]) = 1 + F.length(fa.tail)

  override def index[A](fa: OneAnd[F, A], i: Int) =
    if (i == 0) Some(fa.head) else F.index(fa.tail, i - 1)

  override def toVector[A](fa: OneAnd[F, A]) =
    fa.head +: F.toVector(fa.tail)

  override def toList[A](fa: OneAnd[F, A]) =
    fa.head :: F.toList(fa.tail)

  override def toIList[A](fa: OneAnd[F, A]) =
    fa.head :: F.toIList(fa.tail)

  override def toSet[A](fa: OneAnd[F, A]) =
    F.toSet(fa.tail) + fa.head

  override def toStream[A](fa: OneAnd[F, A]) =
    fa.head #:: F.toStream(fa.tail)

  override def toEphemeralStream[A](fa: OneAnd[F, A]) =
    EphemeralStream.cons(fa.head, F.toEphemeralStream(fa.tail))

  override def all[A](fa: OneAnd[F, A])(f: A => Boolean) =
    f(fa.head) && F.all(fa.tail)(f)

  override def any[A](fa: OneAnd[F, A])(f: A => Boolean) =
    f(fa.head) || F.any(fa.tail)(f)
}

private sealed trait OneAndFoldable1[F[_]] extends OneAndFoldable[F] {
  def F: Foldable1[F]

  override def foldMap1[A, B](fa: OneAnd[F, A])(f: A => B)(implicit S: Semigroup[B]) =
    S.append(f(fa.head), F.foldMap1(fa.tail)(f))

  override def foldMapRight1[A, B](fa: OneAnd[F, A])(z: A => B)(f: (A, => B) => B) =
    f(fa.head, F.foldMapRight1(fa.tail)(z)(f))
}

private sealed trait OneAndTraverse[F[_]] extends Traverse1[OneAnd[F, *]] with OneAndFunctor[F] with OneAndFoldable[F] {
  def F: Traverse[F]

  def traverse1Impl[G[_],A,B](fa: OneAnd[F, A])(f: A => G[B])(implicit G: Apply[G]) =
    G.applyApplicative.traverse(fa.tail)(f andThen \/.left)(F)
     .fold(ftl => G.apply2(f(fa.head), ftl)(OneAnd.apply),
           tl => G.map(f(fa.head))(OneAnd(_, tl)))

  override def traverseImpl[G[_],A,B](fa: OneAnd[F, A])(f: A => G[B])(implicit G: Applicative[G]) =
    G.apply2(f(fa.head), F.traverseImpl(fa.tail)(f)(G))(OneAnd.apply)

  override def traverseS[S,A,B](fa: OneAnd[F, A])(f: A => State[S,B]) =
    State{(s: S) =>
      val (s2, b) = f(fa.head)(s)
      val (s3, bs) = F.traverseS(fa.tail)(f)(s2)
      (s3, OneAnd(b, bs))
    }
}

private sealed trait OneAndTraverse1[F[_]] extends OneAndTraverse[F] with OneAndFoldable1[F] {
  def F: Traverse1[F]

  override def traverse1Impl[G[_],A,B](fa: OneAnd[F, A])(f: A => G[B])(implicit G: Apply[G]) =
    G.apply2(f(fa.head), F.traverse1Impl(fa.tail)(f)(G))(OneAnd.apply)
}

sealed abstract class OneAndInstances5 {
  implicit def oneAndFunctor[F[_]: Functor]: Functor[OneAnd[F, *]] =
    new OneAndFunctor[F] {
      def F = implicitly
    }
}

sealed abstract class OneAndInstances4 extends OneAndInstances5 {
  implicit def oneAndApply[F[_]: Applicative: Plus]: Apply[OneAnd[F, *]] =
    new OneAndApply[F] {
      def F = implicitly
      def G = implicitly
    }

  implicit def oneAndAlign[F[_]: Align]: Align[OneAnd[F, *]] =
    new OneAndAlign[F] {
      def F = implicitly
    }
}

sealed abstract class OneAndInstances3 extends OneAndInstances4 {
  implicit def oneAndApplicative[F[_]: ApplicativePlus]: Applicative[OneAnd[F, *]] =
    new OneAndApplicative[F] {
      def F = implicitly
      def G = implicitly
    }
}

sealed abstract class OneAndInstances2 extends OneAndInstances3 {
  implicit def oneAndBind[F[_]: Monad: Plus]: Bind[OneAnd[F, *]] =
    new OneAndBind[F] {
      def F = implicitly
      def G = implicitly
    }
}

sealed abstract class OneAndInstances1 extends OneAndInstances2 {
  implicit def oneAndMonad[F[_]: MonadPlus]: Monad[OneAnd[F, *]] =
    new OneAndMonad[F] {
      def F = implicitly
    }

  implicit def oneAndFoldable[F[_]: Foldable]: Foldable1[OneAnd[F, *]] =
    new OneAndFoldable[F] {
      def F = implicitly
    }
}

private sealed trait OneAndEqual[F[_], A] extends Equal[OneAnd[F, A]] {
  def OA: Equal[A]
  def OFA: Equal[F[A]]

  override def equal(a1: OneAnd[F, A], a2: OneAnd[F, A]) =
    OA.equal(a1.head, a2.head) && OFA.equal(a1.tail, a2.tail)

  override def equalIsNatural = OA.equalIsNatural && OFA.equalIsNatural
}

sealed abstract class OneAndInstances0 extends OneAndInstances1 {
  /** If you have `Foldable1[F]`, `foldMap1` and `foldRight1` are
    * nonstrict and significantly more efficient.
    */
  implicit def oneAndFoldable1[F[_]: Foldable1]: Foldable1[OneAnd[F, *]] =
    new OneAndFoldable1[F] {
      def F = implicitly
    }

  implicit def oneAndEqual[F[_], A](implicit A: Equal[A], FA: Equal[F[A]]): Equal[OneAnd[F, A]] =
    new OneAndEqual[F, A] {
      def OA = A
      def OFA = FA
    }

  implicit def oneAndTraverse[F[_]: Traverse]: Traverse1[OneAnd[F, *]] =
    new OneAndTraverse[F] {
      def F = implicitly
    }
}

sealed abstract class OneAndInstances extends OneAndInstances0 {
  implicit def oneAndPlus[F[_]: Applicative: Plus]: Plus[OneAnd[F, *]] =
    new OneAndPlus[F] {
      def F = implicitly
      def G = implicitly
    }

  implicit def oneAndTraverse1[F[_]: Traverse1]: Traverse1[OneAnd[F, *]] =
    new OneAndTraverse1[F] {
      def F = implicitly
    }

  implicit def oneAndShow[F[_], A](implicit A: Show[A], FA: Show[F[A]]): Show[OneAnd[F, A]] =
    new Show[OneAnd[F, A]] {
      override def show(f: OneAnd[F, A]) =
        Cord("OneAnd(", A.show(f.head), ",", FA.show(f.tail), ")")
    }

  implicit def oneAndOrder[F[_], A](implicit A: Order[A], FA: Order[F[A]]): Order[OneAnd[F, A]] =
    new Order[OneAnd[F, A]] with OneAndEqual[F, A] {
      def OA: Order[A] = A
      def OFA: Order[F[A]] = FA
      def order(a1: OneAnd[F, A], a2: OneAnd[F, A]) =
        Monoid[Ordering].append(A.order(a1.head, a2.head),
                                FA.order(a1.tail, a2.tail))
    }

  implicit def oneAndSemigroup[F[_]: Applicative: Plus, A]: Semigroup[OneAnd[F, A]] =
    oneAndPlus[F].semigroup

  implicit def oneAndZip[F[_]: Zip]: Zip[OneAnd[F, *]] =
    new Zip[OneAnd[F, *]] {
      def zip[A, B](a: => OneAnd[F, A], b: => OneAnd[F, B]) = {
        val a0 = a
        val b0 = b
        OneAnd((a0.head, b0.head), Zip[F].zip(a0.tail, b0.tail))
      }
    }

  implicit def oneAndUnzip[F[_]: Unzip]: Unzip[OneAnd[F, *]] =
    new Unzip[OneAnd[F, *]] {
      def unzip[A, B](a: OneAnd[F, (A, B)]) = {
        val (fa, fb) = Unzip[F].unzip(a.tail)
        val (aa, ab) = a.head
        (OneAnd(aa, fa), OneAnd(ab, fb))
      }
    }
}

object OneAnd extends OneAndInstances {
  def oneAnd[F[_], A](hd: A, tl: F[A]): OneAnd[F, A] = OneAnd(hd, tl)

  val oneAndNelIso: NonEmptyList <~> OneAnd[List, *] =
    new IsoFunctorTemplate[NonEmptyList, OneAnd[List, *]] {
      def to[A](fa: NonEmptyList[A]) = OneAnd(fa.head, fa.tail.toList)
      def from[A](ga: OneAnd[List, A]) = NonEmptyList.nel(ga.head, IList.fromList(ga.tail))
    }
}
