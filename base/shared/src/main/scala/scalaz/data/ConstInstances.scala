package scalaz
package data


trait ConstInstances {
  implicit def constTraverse[R]: Traversable[Const[R, ?]] = new Traversable.Template[Const[R, ?]] with Traversable.DeriveSequence[Const[R, ?]] with Foldable.DeriveFoldMap[Const[R, ?]] with ConstFunctor[R] {

    override def traverse[F[_], A, B](ta: Const[R, A])(f: A => F[B])(implicit F: Applicative[F]): F[Const[R, B]] =
      F.pure(ta.retag)

    override def foldLeft[A, B](fa: Const[R, A], z: B)(f: (B, A) => B): B = z

    override def foldRight[A, B](fa: Const[R, A], z: => B)(f: (A, => B) => B): B = z

    override def toList[A](fa: Const[R, A]): List[A] = Nil
  }

  private trait ConstFunctor[R] extends Functor.Template[Const[R, ?]] {
    final override def map[A, B](fa: Const[R, A])(f: A => B): Const[R, B] =
      fa.retag[B]
  }

  private trait ConstApply[R] extends Apply.Template[Const[R, ?]] with ConstFunctor[R] {
    def R: Semigroup.Class[R]
    final override def ap[A, B](fa: Const[R, A])(f: Const[R, A => B]): Const[R, B] =
      Const(R.append(fa.getConst, f.getConst))
  }

  private trait ConstApplicative[R] extends Applicative.Template[Const[R, ?]] with ConstApply[R]  {
    override def R: Monoid.Class[R]
    final override def pure[A](a: A): Const[R, A] = Const(R.empty)
  }

  private trait ConstSemigroup[A, B] extends Semigroup.Template[Const[A, B]] {
    def A: Semigroup.Class[A]
    final override def append(a1: Const[A, B], a2: => Const[A, B]): Const[A, B] =
      Const(A.append(a1.getConst, a2.getConst))
  }

  private trait ConstMonoid[A, B] extends Monoid.Template[Const[A, B]] with ConstSemigroup[A, B] {
    override def A: Monoid.Class[A]
    final override def empty: Const[A, B] = Const(A.empty)
  }

  implicit def constApply[R: Semigroup]: Apply[Const[R, ?]] = new ConstApply[R] {
    override val R = implicitly
  }

  implicit def constApplicative[R: Monoid]: Applicative[Const[R, ?]] = new ConstApplicative[R] {
    override val R = implicitly
  }

  implicit def constSemigroup[A: Semigroup, B]: Semigroup[Const[A, B]] = new ConstSemigroup[A, B] {
    override val A = implicitly
  }

  implicit def constMonoid[A: Monoid, B]: Monoid[Const[A, B]] = new ConstMonoid[A, B] {
    override val A = implicitly
  }

  implicit def constShow[A, B](implicit A: Show[A]): Show[Const[A, B]] =
    a => A.show(a.getConst)
}
