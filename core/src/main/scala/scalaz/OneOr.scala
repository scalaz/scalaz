package scalaz

final case class OneOr[F[_], A](run: F[A] \/ A) {
  def map[B](f: A => B)(implicit F: Functor[F]): OneOr[F, B] =
    OneOr(run.bimap(F.map(_)(f), f))

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

  def foldRight1(f: (A, => A) => A)(implicit F: Foldable1[F]): A =
    run match {
      case \/-(a) =>
        a
      case -\/(a) =>
        F.foldRight1(a)(f)
    }

  def foldLeft1(f: (A, A) => A)(implicit F: Foldable1[F]): A =
    run match {
      case \/-(a) =>
        a
      case -\/(a) =>
        F.foldLeft1(a)(f)
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

private[scalaz] sealed trait OneOrFunctor[F[_]]
    extends Functor[({type λ[α] = OneOr[F, α]})#λ] {
  implicit def F: Functor[F]

  override def map[A, B](fa: OneOr[F, A])(f: A => B): OneOr[F, B] =
    fa map f
}

private[scalaz] sealed trait OneOrCobind[F[_]]
    extends OneOrFunctor[F] with Cobind[({type λ[α] = OneOr[F, α]})#λ] {
  implicit def F: Cobind[F]

  override def cobind[A, B](fa: OneOr[F, A])(f: OneOr[F, A] => B): OneOr[F, B] =
    fa cobind f
}

private[scalaz] sealed trait OneOrComonad[F[_]]
    extends OneOrCobind[F] with Comonad[({type λ[α] = OneOr[F, α]})#λ] {
  implicit def F: Comonad[F]

  override def cobind[A, B](fa: OneOr[F, A])(f: OneOr[F, A] => B): OneOr[F, B] =
    fa cobind f

  override def copoint[A](fa: OneOr[F, A]) =
    fa.copoint
}

private[scalaz] sealed trait OneOrApplicative[F[_]]
    extends OneOrFunctor[F] with Applicative[({type λ[α] = OneOr[F, α]})#λ] {
  implicit def F: Apply[F]

  override def ap[A,B](fa: => OneOr[F, A])(f: => OneOr[F, A => B]) =
    fa ap f

  override def point[A](a: => A) =
    OneOr(\/-(a))
}

private[scalaz] sealed trait OneOrFoldable[F[_]]
  extends Foldable[({type λ[α] = OneOr[F, α]})#λ] {

  implicit def F: Foldable[F]

  override def foldMap[A, B](fa: OneOr[F, A])(f: A => B)(implicit M: Monoid[B]) =
    fa.foldMap(f)

  override def foldRight[A, B](fa: OneOr[F, A], z: => B)(f: (A, => B) => B) =
    fa.foldRight(z)(f)

  override def foldLeft[A, B](fa: OneOr[F, A], z: B)(f: (B, A) => B) =
    fa.foldLeft(z)(f)
}

private[scalaz] sealed trait OneOrFoldable1[F[_]]
  extends OneOrFoldable[F] with Foldable1[({type λ[α] = OneOr[F, α]})#λ] {

  implicit def F: Foldable1[F]

  override def foldMap1[A, B](fa: OneOr[F, A])(f: A => B)(implicit M: Semigroup[B]) =
    fa.foldMap1(f)

  override def foldRight1[A](fa: OneOr[F, A])(f: (A, => A) => A) =
    fa.foldRight1(f)

  override def foldLeft1[A](fa: OneOr[F, A])(f: (A, A) => A) =
    fa.foldLeft1(f)
}

private[scalaz] sealed trait OneOrTraverse[F[_]]
  extends OneOrFunctor[F] with OneOrFoldable[F] with Traverse[({type λ[α] = OneOr[F, α]})#λ] {

  implicit def F: Traverse[F]

  override def traverseImpl[G[_]: Applicative,A,B](fa: OneOr[F, A])(f: A => G[B]) =
    fa traverse f

  override def foldMap[A, B](fa: OneOr[F, A])(f: A => B)(implicit M: Monoid[B]) =
    fa.foldMap(f)

}

private[scalaz] sealed trait OneOrTraverse1[F[_]]
  extends OneOrFoldable1[F] with OneOrTraverse[F] with Traverse1[({type λ[α] = OneOr[F, α]})#λ] {

  implicit def F: Traverse1[F]

  override def traverse1Impl[G[_]: Apply,A,B](fa: OneOr[F, A])(f: A => G[B]) =
    fa traverse1 f
}

private[scalaz] sealed trait OneOrEqual[F[_], A] extends Equal[OneOr[F, A]] {
  implicit def OA: Equal[A]
  implicit def OFA: Equal[F[A]]

  override def equal(a1: OneOr[F, A], a2: OneOr[F, A]) =
    a1.run === a2.run

  override def equalIsNatural = OA.equalIsNatural && OFA.equalIsNatural
}

private[scalaz] sealed trait OneOrOrder[F[_], A] extends Order[OneOr[F, A]] {
  implicit def OA: Order[A]
  implicit def OFA: Order[F[A]]

  override def order(a1: OneOr[F, A], a2: OneOr[F, A]) =
    a1.run compare a2.run
}

private[scalaz] sealed trait OneOrShow[F[_], A] extends Show[OneOr[F, A]] {
  implicit def OA: Show[A]
  implicit def OFA: Show[F[A]]

  override def show(a: OneOr[F, A]) =
    a.run.show
}

object OneOr extends OneOrInstances with OneOrFunctions

trait OneOrFunctions {
  type OneOrList[A] = OneOr[List, A]
  type OneOrNel[A] = OneOr[NonEmptyList, A]
  type OneOrOption[A] = OneOr[Option, A]
  type OneOrId[A] = OneOr[Id.Id, A]
  type OneOrNothing[A] = OneOr[Nothing, A]
}

trait OneOrInstances extends OneOrInstances0 {
  implicit def OneOrFunctor[F[_]: Functor]: Functor[({type λ[α] = OneOr[F, α]})#λ] =
    new OneOrFunctor[F] {
      def F = implicitly
    }

  implicit def OneOrEqual[F[_], A](implicit oa: Equal[A], ofa: Equal[F[A]]): Equal[OneOr[F, A]] =
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

trait OneOrInstances0 extends OneOrInstances1 {
  implicit def OneOrCobind[F[_]: Cobind]: Cobind[({type λ[α] = OneOr[F, α]})#λ] =
    new OneOrCobind[F] {
      def F = implicitly
    }

  implicit def OneOrOrder[F[_], A](implicit oa: Order[A], ofa: Order[F[A]]): Order[OneOr[F, A]] =
    new OneOrOrder[F, A] {
      def OA = implicitly
      def OFA = implicitly
    }
}

trait OneOrInstances1 extends OneOrInstances2 {
  implicit def OneOrComonad[F[_]: Comonad]: Comonad[({type λ[α] = OneOr[F, α]})#λ] =
    new OneOrComonad[F] {
      def F = implicitly
    }

}

trait OneOrInstances2 extends OneOrInstances3 {
  implicit def OneOrApplicative[F[_]: Apply]: Applicative[({type λ[α] = OneOr[F, α]})#λ] =
    new OneOrApplicative[F] {
      def F = implicitly
    }
}

trait OneOrInstances3 extends OneOrInstances4 {
  implicit def OneOrFoldable[F[_]: Foldable]: Foldable[({type λ[α] = OneOr[F, α]})#λ] =
    new OneOrFoldable[F] {
      def F = implicitly
    }
}

trait OneOrInstances4 extends OneOrInstances5 {
  implicit def OneOrFoldable1[F[_]: Foldable1]: Foldable1[({type λ[α] = OneOr[F, α]})#λ] =
    new OneOrFoldable1[F] {
      def F = implicitly
    }
}

trait OneOrInstances5 extends OneOrInstances6 {
  implicit def OneOrTraverse[F[_]: Traverse]: Traverse[({type λ[α] = OneOr[F, α]})#λ] =
    new OneOrTraverse[F] {
      def F = implicitly
    }
}

trait OneOrInstances6 {
  implicit def OneOrTraverse1[F[_]: Traverse1]: Traverse1[({type λ[α] = OneOr[F, α]})#λ] =
    new OneOrTraverse1[F] {
      def F = implicitly
    }
}
