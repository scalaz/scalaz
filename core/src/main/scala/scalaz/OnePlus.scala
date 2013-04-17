package scalaz

import scalaz.std.option.{optionMonoid, none, some}
import scalaz.Ordering.orderingInstance

final case class OnePlus[F[_], A](head: A, tail: F[A])

private[scalaz] sealed trait OnePlusFunctor[F[_]]
    extends Functor[({type λ[α] = OnePlus[F, α]})#λ] {
  def F: Functor[F]

  override def map[A, B](fa: OnePlus[F, A])(f: A => B): OnePlus[F, B] =
    OnePlus(f(fa.head), F.map(fa.tail)(f))
}

private[scalaz] sealed trait OnePlusFoldable[F[_]]
    extends Foldable1[({type λ[α] = OnePlus[F, α]})#λ] {
  def F: Foldable[F]

  override def foldMap1[A, B](fa: OnePlus[F, A])(f: A => B)(implicit S: Semigroup[B]) =
    (F.foldMap(fa.tail)(a => some(f(a)))(Monoid[Option[B]])
       map (S.append(f(fa.head), _)) getOrElse f(fa.head))

  override def foldRight1[A](fa: OnePlus[F, A])(f: (A, => A) => A) =
    (F.foldRight(fa.tail, none[A])((a, oa) => some(oa map (f(a, _)) getOrElse a))
       map (f(fa.head, _)) getOrElse fa.head)

  override def foldLeft1[A](fa: OnePlus[F, A])(f: (A, A) => A) =
    F.foldLeft(fa.tail, fa.head)(f)

  override def foldMap[A, B](fa: OnePlus[F, A])(f: A => B)(implicit M: Monoid[B]) =
    M.append(f(fa.head), F.foldMap(fa.tail)(f))

  override def foldRight[A, B](fa: OnePlus[F, A], z: => B)(f: (A, => B) => B) =
    f(fa.head, F.foldRight(fa.tail, z)(f))

  override def foldLeft[A, B](fa: OnePlus[F, A], z: B)(f: (B, A) => B) =
    F.foldLeft(fa.tail, f(z, fa.head))(f)
}

private[scalaz] sealed trait OnePlusFoldable1[F[_]] extends OnePlusFoldable[F] {
  def F: Foldable1[F]

  override def foldMap1[A, B](fa: OnePlus[F, A])(f: A => B)(implicit S: Semigroup[B]) =
    S.append(f(fa.head), F.foldMap1(fa.tail)(f))

  override def foldRight1[A](fa: OnePlus[F, A])(f: (A, => A) => A) =
    f(fa.head, F.foldRight1(fa.tail)(f))
}

/*
private[scalaz] sealed trait OnePlusTraverse[F[_]]
    extends Traverse1[({type λ[α] = OnePlus[F, α]})#λ]
    with OnePlusFunctor[F] with OnePlusFoldable[F] {
  def F: Traverse[F]

  def traverse1Impl[G[_],A,B](fa: OnePlus[F, A])(f: A => G[B])(implicit G: Apply[G]) =
    lift Apply to Applicative, &c
}

private[scalaz] sealed trait OnePlusTraverse1[F[_]]
    extends OnePlusTraverse[F] {
   def F: Traverse1[F]
*/

trait OnePlusInstances1 {
  implicit def onePlusFunctor[F[_]: Functor]: Functor[({type λ[α] = OnePlus[F, α]})#λ] =
    new OnePlusFunctor[F] {
      def F = implicitly
    }

  implicit def onePlusFoldable[F[_]: Foldable]: Foldable1[({type λ[α] = OnePlus[F, α]})#λ] =
    new OnePlusFoldable[F] {
      def F = implicitly
    }
}

private[scalaz]
sealed trait OnePlusEqual[F[_], A] extends Equal[OnePlus[F, A]] {
  def OA: Equal[A]
  def OFA: Equal[F[A]]

  override def equal(a1: OnePlus[F, A], a2: OnePlus[F, A]) =
    OA.equal(a1.head, a2.head) && OFA.equal(a1.tail, a2.tail)

  override def equalIsNatural = OA.equalIsNatural && OFA.equalIsNatural
}

trait OnePlusInstances0 extends OnePlusInstances1 {
  /** If you have `Foldable1[F]`, `foldMap1` and `foldRight1` are
    * nonstrict and significantly more efficient.
    */
  implicit def onePlusFoldable1[F[_]: Foldable1]: Foldable1[({type λ[α] = OnePlus[F, α]})#λ] =
    new OnePlusFoldable1[F] {
      def F = implicitly
    }

  implicit def onePlusEqual[F[_], A](implicit A: Equal[A], FA: Equal[F[A]]): Equal[OnePlus[F, A]] =
    new OnePlusEqual[F, A] {
      def OA = A
      def OFA = FA
    }
  // TODO 1 + traverse
}

trait OnePlusInstances extends OnePlusInstances0 {
  // TODO 1 + traverse1

  implicit def onePlusShow[F[_], A](implicit A: Show[A], FA: Show[F[A]]): Show[OnePlus[F, A]] =
    new Show[OnePlus[F, A]] {
      override def show(f: OnePlus[F, A]) =
        Cord("OnePlus(", A.show(f.head), ",", FA.show(f.tail), ")")
    }

  implicit def onePlusOrder[F[_], A](implicit A: Order[A], FA: Order[F[A]]): Order[OnePlus[F, A]] =
    new Order[OnePlus[F, A]] with OnePlusEqual[F, A] {
      def OA = A
      def OFA = FA
      def order(a1: OnePlus[F, A], a2: OnePlus[F, A]) =
        Monoid[Ordering].append(A.order(a1.head, a2.head),
                                FA.order(a1.tail, a2.tail))
    }

  implicit def onePlusEach[F[_]: Each]: Each[({type λ[α] = OnePlus[F, α]})#λ] =
    new Each[({type λ[α] = OnePlus[F, α]})#λ] {
      def each[A](fa: OnePlus[F, A])(f: A => Unit) = {
        f(fa.head)
        Each[F].each(fa.tail)(f)
      }
    }

  implicit def onePlusLength[F[_]: Length, A]: Length[({type λ[α] = OnePlus[F, α]})#λ] =
    new Length[({type λ[α] = OnePlus[F, α]})#λ] {
      def length[A](fa: OnePlus[F, A]) = 1 + Length[F].length(fa.tail)
    }

  implicit def onePlusZip[F[_]: Zip]: Zip[({type λ[α] = OnePlus[F, α]})#λ] =
    new Zip[({type λ[α] = OnePlus[F, α]})#λ] {
      def zip[A, B](a: => OnePlus[F, A], b: => OnePlus[F, B]) =
        OnePlus((a.head, b.head), Zip[F].zip(a.tail, b.tail))
    }

  implicit def onePlusUnzip[F[_]: Unzip]: Unzip[({type λ[α] = OnePlus[F, α]})#λ] =
    new Unzip[({type λ[α] = OnePlus[F, α]})#λ] {
      def unzip[A, B](a: OnePlus[F, (A, B)]) = {
        val (fa, fb) = Unzip[F].unzip(a.tail)
        (OnePlus(a.head._1, fa), OnePlus(a.head._2, fb))
      }
    }
}

object OnePlus extends OnePlusFunctions with OnePlusInstances

trait OnePlusFunctions {
  def oneAnd[F[_], A](hd: A, tl: F[A]): OnePlus[F, A] = OnePlus(hd, tl)
}
