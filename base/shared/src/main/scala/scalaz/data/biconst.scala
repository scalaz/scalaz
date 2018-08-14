package scalaz
package data

import tc._

abstract class BiconstModule {
  type Biconst[A, B, C]

  def apply[A, B, C](a: A): Biconst[A, B, C]
  def run[A, B, C](const: Biconst[A, B, C]): A

  def biconstSemicategory[A: Semigroup]: Semicategory[Biconst[A, ?, ?]]
  def biconstCategory[A: Monoid]: Category[Biconst[A, ?, ?]]
  def biconstApply[A: Semigroup, B]: Apply[Biconst[A, B, ?]]
  def biconstApplicative[A: Monoid, B]: Applicative[Biconst[A, B, ?]]
  def biconstTraversable[A, B]: Traversable[Biconst[A, B, ?]]
  def biconstBifunctor[A]: Bifunctor[Biconst[A, ?, ?]]
  def biconstPhantom[A, B]: Phantom[Biconst[A, B, ?]]

  def biconstSemigroup[A: Semigroup, B, C]: Semigroup[Biconst[A, B, C]]
  def biconstMonoid[A: Monoid, B, C]: Monoid[Biconst[A, B, C]]
  def biconstEq[A: Eq, B, C]: Eq[Biconst[A, B, C]]
  def biconstDebug[A: Debug, B, C]: Debug[Biconst[A, B, C]]
}

object BiconstModule {
  implicit def biconstSemicategory[A: Semigroup]: Semicategory[Biconst[A, ?, ?]] =
    Biconst.biconstSemicategory[A]
  implicit def biconstCategory[A: Monoid]: Category[Biconst[A, ?, ?]] =
    Biconst.biconstCategory[A]
  implicit def biconstApply[A: Semigroup, B]: Apply[Biconst[A, B, ?]] =
    Biconst.biconstApply[A, B]
  implicit def biconstApplicative[A: Monoid, B]: Applicative[Biconst[A, B, ?]] =
    Biconst.biconstApplicative[A, B]
  implicit def biconstTraversable[A, B]: Traversable[Biconst[A, B, ?]] =
    Biconst.biconstTraversable[A, B]
  implicit def biconstPhantom[A, B]: Phantom[Biconst[A, B, ?]] =
    Biconst.biconstPhantom[A, B]

  implicit def biconstSemigroup[A: Semigroup, B, C]: Semigroup[Biconst[A, B, C]] =
    Biconst.biconstSemigroup[A, B, C]
  implicit def biconstMonoid[A: Monoid, B, C]: Monoid[Biconst[A, B, C]] =
    Biconst.biconstMonoid[A, B, C]
  implicit def biconstEq[A: Eq, B, C]: Eq[Biconst[A, B, C]] =
    Biconst.biconstEq[A, B, C]
  implicit def biconstDebug[A: Debug, B, C]: Debug[Biconst[A, B, C]] =
    Biconst.biconstDebug[A, B, C]
}

private[data] object BiconstImpl extends BiconstModule {
  type Biconst[A, B, C] = A

  def apply[A, B, C](a: A): A     = a
  def run[A, B, C](biconst: A): A = biconst

  private trait BiconstSemicategory[A] extends SemicategoryClass[Biconst[A, ?, ?]] {
    def A: SemigroupClass[A]
    def compose[B, C, D](f: A, g: A): A = A.mappend(f, g)
  }

  private trait BiconstPhantom[R, A]
      extends PhantomClass[Biconst[R, A, ?]]
      with PhantomClass.DeriveMapContramap[Biconst[R, A, ?]] {
    def pmap[B, C](ma: Biconst[R, A, B]): Biconst[R, A, C] = ma
  }

  private trait BiconstApply[R, A] extends ApplyClass[Biconst[R, A, ?]] with BiconstPhantom[R, A] {
    def R: SemigroupClass[R]

    final override def ap[B, C](fa: Biconst[R, A, B])(f: Biconst[R, A, B => C]): Biconst[R, A, C] =
      R.mappend(fa, f)
  }

  private trait BiconstApplicative[R, A] extends ApplicativeClass[Biconst[R, A, ?]] with BiconstApply[R, A] {
    override def R: MonoidClass[R]
    final override def pure[B](b: B): Biconst[R, A, B] = R.mempty
  }

  def biconstSemicategory[A](implicit A0: Semigroup[A]): Semicategory[Biconst[A, ?, ?]] =
    instanceOf(new BiconstSemicategory[A] { def A = A0 })
  def biconstCategory[A](implicit A0: Monoid[A]): Category[Biconst[A, ?, ?]] =
    instanceOf(new BiconstSemicategory[A] with CategoryClass[Biconst[A, ?, ?]] {
      def A        = A0
      def id[X]: A = A0.mempty
    })
  def biconstApply[A: Semigroup, B]: Apply[Biconst[A, B, ?]] =
    instanceOf(new BiconstApply[A, B] {
      val R = Semigroup[A]
    })
  def biconstApplicative[A: Monoid, B]: Applicative[Biconst[A, B, ?]] =
    instanceOf(new BiconstApplicative[A, B] {
      val R = Monoid[A]
    })

  def biconstTraversable[C, D]: Traversable[Biconst[C, D, ?]] =
    instanceOf(new TraversableClass[Biconst[C, D, ?]] with BiconstPhantom[C, D] {
      def foldLeft[A, B](fa: C, z: B)(f: (B, A) => B): B                                        = z
      def foldMap[A, B](fa: C)(f: A => B)(implicit B: Monoid[B]): B                             = B.mempty
      def foldRight[A, B](fa: C, z: => B)(f: (A, => B) => B): B                                 = z
      def msuml[A](fa: C)(implicit A: Monoid[A]): A                                             = A.mempty
      def toList[A](fa: C): scala.List[A]                                                       = scala.Nil
      def sequence[F[_], A](ta: C)(implicit F: scalaz.tc.Applicative[F]): F[C]                  = F.pure(ta)
      def traverse[F[_], A, B](ta: C)(f: A => F[B])(implicit F: scalaz.tc.Applicative[F]): F[C] = F.pure(ta)
    })

  def biconstBifunctor[A]: Bifunctor[Biconst[A, ?, ?]] =
    instanceOf(new BifunctorClass[Biconst[A, ?, ?]] {
      def bimap[B, C, S, T](fab: A)(as: B => S, bt: C => T): A = fab
      def lmap[B, C, S](fab: A)(as: B => S): A                 = fab
      def rmap[B, C, T](fab: A)(bt: C => T): A                 = fab
    })

  def biconstPhantom[A, B]: Phantom[Biconst[A, B, ?]] =
    instanceOf(new BiconstPhantom[A, B] {})

  def biconstSemigroup[A, B, C](implicit A: Semigroup[A]): Semigroup[Biconst[A, B, C]] =
    A
  def biconstMonoid[A, B, C](implicit A: Monoid[A]): Monoid[Biconst[A, B, C]] =
    A
  def biconstEq[A, B, C](implicit A: Eq[A]): Eq[Biconst[A, B, C]] =
    A
  def biconstDebug[A, B, C](implicit A: Debug[A]): Debug[Biconst[A, B, C]] =
    A
}
