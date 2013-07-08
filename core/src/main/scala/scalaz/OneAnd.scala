package scalaz

import scalaz.Isomorphism.{<~>, IsoFunctorTemplate}
import scalaz.std.option.{optionMonoid, none, some}
import scalaz.Ordering.orderingInstance

final case class OneAnd[F[_], A](head: A, tail: F[A])

private[scalaz] sealed trait OneAndFunctor[F[_]]
    extends Functor[({type λ[α] = OneAnd[F, α]})#λ] {
  def F: Functor[F]

  override def map[A, B](fa: OneAnd[F, A])(f: A => B): OneAnd[F, B] =
    OneAnd(f(fa.head), F.map(fa.tail)(f))
}

private[scalaz] sealed trait OneAndApply[F[_]] extends Apply[({type λ[α] = OneAnd[F, α]})#λ] with OneAndFunctor[F] {
  def F: Applicative[F]
  def G: Plus[F]

  override def ap[A, B](fa: => OneAnd[F, A])(f: => OneAnd[F, A => B]): OneAnd[F, B] = {
    val OneAnd(hf, tf) = f
    val OneAnd(ha, ta) = fa
    OneAnd(hf(ha), G.plus(F.map(ta)(hf),
                          F.ap(G.plus(F.point(ha), ta))(tf)))
  }
}

private[scalaz] sealed trait OneAndApplicative[F[_]] extends Applicative[({type λ[α] = OneAnd[F, α]})#λ] with OneAndApply[F] {
  def F: ApplicativePlus[F]

  def point[A](a: => A): OneAnd[F, A] = OneAnd(a, F.empty)
}

private[scalaz] sealed trait OneAndBind[F[_]] extends Bind[({type λ[α] = OneAnd[F, α]})#λ] with OneAndApply[F] {
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

private[scalaz] sealed trait OneAndPlus[F[_]] extends Plus[({type λ[α] = OneAnd[F, α]})#λ] {
  def F: Applicative[F]
  def G: Plus[F]

  def plus[A](a: OneAnd[F, A], b: => OneAnd[F, A]): OneAnd[F, A] =
    OneAnd(a.head, G.plus(G.plus(a.tail, F.point(b.head)), b.tail))
}

private[scalaz] sealed trait OneAndMonad[F[_]] extends Monad[({type λ[α] = OneAnd[F, α]})#λ] with OneAndBind[F] with OneAndApplicative[F] {
  def F: MonadPlus[F]
  def G = F
}

private[scalaz] sealed trait OneAndFoldable[F[_]]
    extends Foldable1[({type λ[α] = OneAnd[F, α]})#λ] {
  def F: Foldable[F]

  override def foldMap1[A, B: Semigroup](fa: OneAnd[F, A])(f: A => B) =
    foldMap(fa)(a => some(f(a))) getOrElse f(fa.head)

  override def foldRight1[A](fa: OneAnd[F, A])(f: (A, => A) => A) =
    (F.foldRight(fa.tail, none[A])((a, oa) => oa map (f(a, _)) orElse some(a))
       map (f(fa.head, _)) getOrElse fa.head)

  override def foldLeft1[A](fa: OneAnd[F, A])(f: (A, A) => A) =
    F.foldLeft(fa.tail, fa.head)(f)

  override def foldMap[A, B](fa: OneAnd[F, A])(f: A => B)(implicit M: Monoid[B]) =
    M.append(f(fa.head), F.foldMap(fa.tail)(f))

  override def foldRight[A, B](fa: OneAnd[F, A], z: => B)(f: (A, => B) => B) =
    f(fa.head, F.foldRight(fa.tail, z)(f))

  override def foldLeft[A, B](fa: OneAnd[F, A], z: B)(f: (B, A) => B) =
    F.foldLeft(fa.tail, f(z, fa.head))(f)
}

private[scalaz] sealed trait OneAndFoldable1[F[_]] extends OneAndFoldable[F] {
  def F: Foldable1[F]

  override def foldMap1[A, B](fa: OneAnd[F, A])(f: A => B)(implicit S: Semigroup[B]) =
    S.append(f(fa.head), F.foldMap1(fa.tail)(f))

  override def foldRight1[A](fa: OneAnd[F, A])(f: (A, => A) => A) =
    f(fa.head, F.foldRight1(fa.tail)(f))
}

private[scalaz] sealed trait OneAndTraverse[F[_]]
    extends Traverse1[({type λ[α] = OneAnd[F, α]})#λ]
    with OneAndFunctor[F] with OneAndFoldable[F] {
  def F: Traverse[F]

  def traverse1Impl[G[_],A,B](fa: OneAnd[F, A])(f: A => G[B])(implicit G: Apply[G]) =
    (F.traverseImpl[({type λ[α] = G[α] \/ α})#λ, A, B
                   ](fa.tail)(a => -\/(f(a)))(G.applyApplicative)
       fold (ftl => G.apply2(f(fa.head), ftl)(OneAnd.apply),
             tl => G.map(f(fa.head))(OneAnd(_, tl))))

  override def traverseImpl[G[_],A,B](fa: OneAnd[F, A])(f: A => G[B])(implicit G: Applicative[G]) =
    G.apply2(f(fa.head), F.traverseImpl(fa.tail)(f)(G))(OneAnd.apply)
}

private[scalaz] sealed trait OneAndTraverse1[F[_]]
    extends OneAndTraverse[F] with OneAndFoldable1[F] {
  def F: Traverse1[F]

  override def traverse1Impl[G[_],A,B](fa: OneAnd[F, A])(f: A => G[B])(implicit G: Apply[G]) =
    G.apply2(f(fa.head), F.traverse1Impl(fa.tail)(f)(G))(OneAnd.apply)
}

trait OneAndInstances5 {
  implicit def oneAndFunctor[F[_]: Functor]: Functor[({type λ[α] = OneAnd[F, α]})#λ] =
    new OneAndFunctor[F] {
      def F = implicitly
    }
}

trait OneAndInstances4 extends OneAndInstances5 {
  implicit def oneAndApply[F[_]: Applicative: Plus]: Apply[({type λ[α] = OneAnd[F, α]})#λ] =
    new OneAndApply[F] {
      def F = implicitly
      def G = implicitly
    }
}

trait OneAndInstances3 extends OneAndInstances4 {
  implicit def oneAndApplicative[F[_]: ApplicativePlus]: Applicative[({type λ[α] = OneAnd[F, α]})#λ] =
    new OneAndApplicative[F] {
      def F = implicitly
      def G = implicitly
    }
}

trait OneAndInstances2 extends OneAndInstances3 {
  implicit def oneAndBind[F[_]: Monad: Plus]: Bind[({type λ[α] = OneAnd[F, α]})#λ] =
    new OneAndBind[F] {
      def F = implicitly
      def G = implicitly
    }
}

trait OneAndInstances1 extends OneAndInstances2 {
  implicit def oneAndMonad[F[_]: MonadPlus]: Monad[({type λ[α] = OneAnd[F, α]})#λ] =
    new OneAndMonad[F] {
      def F = implicitly
    }

  implicit def oneAndFoldable[F[_]: Foldable]: Foldable1[({type λ[α] = OneAnd[F, α]})#λ] =
    new OneAndFoldable[F] {
      def F = implicitly
    }
}

private[scalaz]
sealed trait OneAndEqual[F[_], A] extends Equal[OneAnd[F, A]] {
  def OA: Equal[A]
  def OFA: Equal[F[A]]

  override def equal(a1: OneAnd[F, A], a2: OneAnd[F, A]) =
    OA.equal(a1.head, a2.head) && OFA.equal(a1.tail, a2.tail)

  override def equalIsNatural = OA.equalIsNatural && OFA.equalIsNatural
}

trait OneAndInstances0 extends OneAndInstances1 {
  /** If you have `Foldable1[F]`, `foldMap1` and `foldRight1` are
    * nonstrict and significantly more efficient.
    */
  implicit def oneAndFoldable1[F[_]: Foldable1]: Foldable1[({type λ[α] = OneAnd[F, α]})#λ] =
    new OneAndFoldable1[F] {
      def F = implicitly
    }

  implicit def oneAndEqual[F[_], A](implicit A: Equal[A], FA: Equal[F[A]]): Equal[OneAnd[F, A]] =
    new OneAndEqual[F, A] {
      def OA = A
      def OFA = FA
    }

  implicit def oneAndTraverse[F[_]: Traverse]: Traverse1[({type λ[α] = OneAnd[F, α]})#λ] =
    new OneAndTraverse[F] {
      def F = implicitly
    }
}

trait OneAndInstances extends OneAndInstances0 {
  implicit def oneAndPlus[F[_]: Applicative: Plus]: Plus[({type λ[α] = OneAnd[F, α]})#λ] =
    new OneAndPlus[F] {
      def F = implicitly
      def G = implicitly
    }

  implicit def oneAndTraverse1[F[_]: Traverse1]: Traverse1[({type λ[α] = OneAnd[F, α]})#λ] =
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
      def OA = A
      def OFA = FA
      def order(a1: OneAnd[F, A], a2: OneAnd[F, A]) =
        Monoid[Ordering].append(A.order(a1.head, a2.head),
                                FA.order(a1.tail, a2.tail))
    }

  implicit def oneAndSemigroup[F[_]: Applicative: Plus, A]: Semigroup[OneAnd[F, A]] =
    oneAndPlus[F].semigroup

  @deprecated("Each is deprecated", "7.1")
  implicit def oneAndEach[F[_]: Each]: Each[({type λ[α] = OneAnd[F, α]})#λ] =
    new Each[({type λ[α] = OneAnd[F, α]})#λ] {
      def each[A](fa: OneAnd[F, A])(f: A => Unit) = {
        f(fa.head)
        Each[F].each(fa.tail)(f)
      }
    }

  @deprecated("Length is deprecated", "7.1")
  implicit def oneAndLength[F[_]: Length, A]: Length[({type λ[α] = OneAnd[F, α]})#λ] =
    new Length[({type λ[α] = OneAnd[F, α]})#λ] {
      def length[A](fa: OneAnd[F, A]) = 1 + Length[F].length(fa.tail)
    }

  implicit def oneAndZip[F[_]: Zip]: Zip[({type λ[α] = OneAnd[F, α]})#λ] =
    new Zip[({type λ[α] = OneAnd[F, α]})#λ] {
      def zip[A, B](a: => OneAnd[F, A], b: => OneAnd[F, B]) =
        OneAnd((a.head, b.head), Zip[F].zip(a.tail, b.tail))
    }

  implicit def oneAndUnzip[F[_]: Unzip]: Unzip[({type λ[α] = OneAnd[F, α]})#λ] =
    new Unzip[({type λ[α] = OneAnd[F, α]})#λ] {
      def unzip[A, B](a: OneAnd[F, (A, B)]) = {
        val (fa, fb) = Unzip[F].unzip(a.tail)
        (OneAnd(a.head._1, fa), OneAnd(a.head._2, fb))
      }
    }
}

object OneAnd extends OneAndFunctions with OneAndInstances

trait OneAndFunctions {
  def oneAnd[F[_], A](hd: A, tl: F[A]): OneAnd[F, A] = OneAnd(hd, tl)

  val oneAndNelIso: NonEmptyList <~> ({type λ[α] = OneAnd[List, α]})#λ =
    new IsoFunctorTemplate[NonEmptyList, ({type λ[α] = OneAnd[List, α]})#λ] {
      def to[A](fa: NonEmptyList[A]) = OneAnd(fa.head, fa.tail)
      def from[A](ga: OneAnd[List, A]) = NonEmptyList.nel(ga.head, ga.tail)
    }
}
