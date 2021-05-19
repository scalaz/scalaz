package scalaz

/** @since 7.0.3 */
final case class OneOr[F[_], A](run: F[A] \/ A) {
  def map[B](f: A => B)(implicit F: Functor[F]): OneOr[F, B] =
    OneOr(run.bimap(F.lift(f), f))

  def ap[B](f: OneOr[F, A => B])(implicit F: Apply[F]): OneOr[F, B] =
    OneOr(f.run match {
      case -\/(g) =>
        run match {
          case -\/(h) =>
            -\/(F.ap(h)(g))
          case \/-(h) =>
            -\/(F.map(g)(_(h)))
        }
      case \/-(g) =>
        run match {
          case -\/(h) =>
            -\/(F.map(h)(g))
          case \/-(h) =>
            \/-(g(h))
        }
    })

  def cojoin(implicit F: Cobind[F]): OneOr[F, OneOr[F, A]] =
    OneOr(run match {
      case \/-(_) =>
        \/-(this)
      case -\/(a) =>
        -\/(F.extend(a)(t => OneOr(-\/(t))))
    })

  def cobind[B](f: OneOr[F, A] => B)(implicit F: Cobind[F]): OneOr[F, B] =
    OneOr(run match {
      case \/-(_) =>
        \/-(f(this))
      case -\/(a) =>
        -\/(F.cobind(a)(t => f(OneOr(-\/(t)))))
    })

  def copoint(implicit F: Comonad[F]): A =
    run valueOr F.copoint

  def foldMap[B](f: A => B)(implicit M: Monoid[B], F: Foldable[F]): B =
    run match {
      case \/-(a) =>
        f(a)
      case -\/(a) =>
        F.foldMap(a)(f)
    }

  def foldRight[B](z: => B)(f: (A, => B) => B)(implicit F: Foldable[F]): B =
    run match {
      case \/-(a) =>
        f(a, z)
      case -\/(a) =>
        F.foldRight(a, z)(f)
    }

  def foldLeft[B](z: => B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    run match {
      case \/-(a) =>
        f(z, a)
      case -\/(a) =>
        F.foldLeft(a, z)(f)
    }

  def foldMap1[B](f: A => B)(implicit M: Semigroup[B], F: Foldable1[F]): B =
    run match {
      case \/-(a) =>
        f(a)
      case -\/(a) =>
        F.foldMap1(a)(f)
    }

  def foldMapRight1[B](z: A => B)(f: (A, => B) => B)(implicit F: Foldable1[F]): B =
    run match {
      case \/-(a) =>
        z(a)
      case -\/(a) =>
        F.foldMapRight1(a)(z)(f)
    }

  def foldMapLeft1[B](z: A => B)(f: (B, A) => B)(implicit F: Foldable1[F]): B =
    run match {
      case \/-(a) =>
        z(a)
      case -\/(a) =>
        F.foldMapLeft1(a)(z)(f)
    }

  def traverse[G[_], B](f: A => G[B])(implicit T: Traverse[F], F: Applicative[G]): G[OneOr[F, B]] =
    run match {
      case \/-(a) =>
        F.map(f(a))(t => OneOr(\/-(t)))
      case -\/(a) =>
        F.map(T.traverse(a)(f))(t => OneOr(-\/(t)))
    }

  def traverse1[G[_], B](f: A => G[B])(implicit T: Traverse1[F], F: Apply[G]): G[OneOr[F, B]] =
    run match {
      case \/-(a) =>
        F.map(f(a))(t => OneOr(\/-(t)))
      case -\/(a) =>
        F.map(T.traverse1(a)(f))(t => OneOr(-\/(t)))
    }


}

private sealed trait OneOrFunctor[F[_]] extends Functor[OneOr[F, *]] {
  implicit def F: Functor[F]

  override final def map[A, B](fa: OneOr[F, A])(f: A => B): OneOr[F, B] =
    fa map f
}

private sealed trait OneOrCobind[F[_]] extends Cobind[OneOr[F, *]] with OneOrFunctor[F]{
  implicit def F: Cobind[F]

  override final def cobind[A, B](fa: OneOr[F, A])(f: OneOr[F, A] => B): OneOr[F, B] =
    fa cobind f
}

private sealed trait OneOrComonad[F[_]] extends OneOrCobind[F] with Comonad[OneOr[F, *]] {
  implicit def F: Comonad[F]

  override def copoint[A](fa: OneOr[F, A]) =
    fa.copoint
}

private sealed trait OneOrApplicative[F[_]] extends Applicative[OneOr[F, *]] with OneOrFunctor[F]{
  implicit def F: Apply[F]

  override final def ap[A,B](fa: => OneOr[F, A])(f: => OneOr[F, A => B]) =
    fa ap f

  override def point[A](a: => A) =
    OneOr(\/-(a))
}

private sealed trait OneOrFoldable[F[_]] extends Foldable[OneOr[F, *]] {

  implicit def F: Foldable[F]

  override final def findLeft[A](fa: OneOr[F, A])(f: A => Boolean) =
    fa.run match {
      case \/-(a) =>
        if(f(a)) Some(a) else None
      case -\/(a) =>
        F.findLeft(a)(f)
    }

  override final def findRight[A](fa: OneOr[F, A])(f: A => Boolean) =
    fa.run match {
      case \/-(a) =>
        if(f(a)) Some(a) else None
      case -\/(a) =>
        F.findRight(a)(f)
    }

  override final def foldMap[A, B](fa: OneOr[F, A])(f: A => B)(implicit M: Monoid[B]) =
    fa.foldMap(f)

  override final def foldRight[A, B](fa: OneOr[F, A], z: => B)(f: (A, => B) => B) =
    fa.foldRight(z)(f)

  override final def foldLeft[A, B](fa: OneOr[F, A], z: B)(f: (B, A) => B) =
    fa.foldLeft(z)(f)
}

private sealed trait OneOrFoldable1[F[_]] extends Foldable1[OneOr[F, *]] with OneOrFoldable[F]{

  implicit def F: Foldable1[F]

  override final def foldMap1[A, B](fa: OneOr[F, A])(f: A => B)(implicit M: Semigroup[B]) =
    fa.foldMap1(f)

  override final def foldMapRight1[A, B](fa: OneOr[F, A])(z: A => B)(f: (A, => B) => B) =
    fa.foldMapRight1(z)(f)

  override final def foldMapLeft1[A, B](fa: OneOr[F, A])(z: A => B)(f: (B, A) => B) =
    fa.foldMapLeft1(z)(f)
}

private sealed trait OneOrTraverse[F[_]] extends Traverse[OneOr[F, *]] with OneOrFunctor[F] with OneOrFoldable[F] {

  implicit def F: Traverse[F]

  override final def traverseImpl[G[_]: Applicative,A,B](fa: OneOr[F, A])(f: A => G[B]) =
    fa traverse f

}

private sealed trait OneOrTraverse1[F[_]] extends Traverse1[OneOr[F, *]] with OneOrFoldable1[F] with OneOrTraverse[F] {

  implicit def F: Traverse1[F]

  override def traverse1Impl[G[_]: Apply,A,B](fa: OneOr[F, A])(f: A => G[B]) =
    fa traverse1 f
}

private sealed trait OneOrEqual[F[_], A] extends Equal[OneOr[F, A]] {
  implicit def OA: Equal[A]
  implicit def OFA: Equal[F[A]]

  override final def equal(a1: OneOr[F, A], a2: OneOr[F, A]) =
    a1.run === a2.run

  override final def equalIsNatural = OA.equalIsNatural && OFA.equalIsNatural
}

private sealed trait OneOrOrder[F[_], A] extends Order[OneOr[F, A]]
  with OneOrEqual[F, A] {
  implicit def OA: Order[A]
  implicit def OFA: Order[F[A]]

  override def order(a1: OneOr[F, A], a2: OneOr[F, A]) =
    a1.run compare a2.run
}

private sealed trait OneOrShow[F[_], A] extends Show[OneOr[F, A]] {
  implicit def OA: Show[A]
  implicit def OFA: Show[F[A]]

  override def show(a: OneOr[F, A]) =
    a.run.show
}

object OneOr extends OneOrInstances {
  type OneOrList[A] = OneOr[List, A]
  type OneOrNel[A] = OneOr[NonEmptyList, A]
  type OneOrOption[A] = OneOr[Option, A]
  type OneOrId[A] = OneOr[Id.Id, A]
  type OneOrNothing[A] = OneOr[Nothing, A]
}

sealed abstract class OneOrInstances extends OneOrInstances0 {
  // for binary compatibility
  def OneOrFunctor[F[_]: Functor]: Functor[OneOr[F, *]] =
    new OneOrFunctor[F] {
      def F = implicitly
    }

  // for binary compatibility
  def OneOrEqual[F[_], A](implicit oa: Equal[A], ofa: Equal[F[A]]): Equal[OneOr[F, A]] =
    new OneOrEqual[F, A] {
      def OA = implicitly
      def OFA = implicitly
    }

  implicit def OneOrShow[F[_], A](implicit oa: Show[A], ofa: Show[F[A]]): Show[OneOr[F, A]] =
    new OneOrShow[F, A] {
      def OA = implicitly
      def OFA = implicitly
    }
}

sealed abstract class OneOrInstances0 extends OneOrInstances1 {
  // for binary compatibility
  def OneOrCobind[F[_]: Cobind]: Cobind[OneOr[F, *]] =
    new OneOrCobind[F] {
      def F = implicitly
    }

  implicit def OneOrComonad0[F[_]: Comonad]: Comonad[OneOr[F, *]] =
    new OneOrComonad[F] {
      def F = implicitly
    }

  implicit def OneOrOrder[F[_], A](implicit oa: Order[A], ofa: Order[F[A]]): Order[OneOr[F, A]] =
    new OneOrOrder[F, A] {
      def OA = implicitly
      def OFA = implicitly
    }
}

sealed abstract class OneOrInstances1 extends OneOrInstances2 {
  implicit def OneOrCobind0[F[_]: Cobind]: Cobind[OneOr[F, *]] =
    new OneOrCobind[F] {
      def F = implicitly
    }

  // for binary compatibility
  def OneOrComonad[F[_]: Comonad]: Comonad[OneOr[F, *]] =
    new OneOrComonad[F] {
      def F = implicitly
    }

  implicit def OneOrEqual0[F[_], A](implicit oa: Equal[A], ofa: Equal[F[A]]): Equal[OneOr[F, A]] =
    new OneOrEqual[F, A] {
      def OA = implicitly
      def OFA = implicitly
    }
}

sealed abstract class OneOrInstances2 extends OneOrInstances3 {
  implicit def OneOrApplicative[F[_]: Apply]: Applicative[OneOr[F, *]] =
    new OneOrApplicative[F] {
      def F = implicitly
    }
}

sealed abstract class OneOrInstances3 extends OneOrInstances4 {
  // for binary compatibility
  def OneOrFoldable[F[_]: Foldable]: Foldable[OneOr[F, *]] =
    new OneOrFoldable[F] {
      def F = implicitly
    }
  implicit def OneOrTraverse1_0[F[_]: Traverse1]: Traverse1[OneOr[F, *]] =
    new OneOrTraverse1[F] {
      def F = implicitly
    }
}

sealed abstract class OneOrInstances4 extends OneOrInstances5 {
  // for binary compatibility
  def OneOrFoldable1[F[_]: Foldable1]: Foldable1[OneOr[F, *]] =
    new OneOrFoldable1[F] {
      def F = implicitly
    }
  implicit def OneOrTraverse0[F[_]: Traverse]: Traverse[OneOr[F, *]] =
    new OneOrTraverse[F] {
      def F = implicitly
    }
}

sealed abstract class OneOrInstances5 extends OneOrInstances6 {
  // for binary compatibility
  def OneOrTraverse[F[_]: Traverse]: Traverse[OneOr[F, *]] =
    new OneOrTraverse[F] {
      def F = implicitly
    }
  implicit def OneOrFoldable1_0[F[_]: Foldable1]: Foldable1[OneOr[F, *]] =
    new OneOrFoldable1[F] {
      def F = implicitly
    }
}

sealed abstract class OneOrInstances6 {
  // for binary compatibility
  def OneOrTraverse1[F[_]: Traverse1]: Traverse1[OneOr[F, *]] =
    new OneOrTraverse1[F] {
      def F = implicitly
    }
  implicit def OneOrFoldable0[F[_]: Foldable]: Foldable[OneOr[F, *]] =
    new OneOrFoldable[F] {
      def F = implicitly
    }
  implicit def OneOrFunctor0[F[_]: Functor]: Functor[OneOr[F, *]] =
    new OneOrFunctor[F] {
      def F = implicitly
    }
}
