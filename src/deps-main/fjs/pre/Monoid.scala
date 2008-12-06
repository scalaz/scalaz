package fjs.pre

object Monoid {
  def monoid[A](zero: A)(implicit s: fj.pre.Semigroup[A]): fj.pre.Monoid[A] =
    fj.pre.Monoid.monoid(s, zero)

  implicit val intAdditionMonoid = fj.pre.Monoid.intAdditionMonoid
  implicit val intMultiplicationMonoid = fj.pre.Monoid.intMultiplicationMonoid
  implicit val disjunctionMonoid = fj.pre.Monoid.disjunctionMonoid
  implicit val conjunctionMonoid = fj.pre.Monoid.conjunctionMonoid
  implicit val stringMonoid = fj.pre.Monoid.stringMonoid
  implicit val stringBufferMonoid = fj.pre.Monoid.stringBufferMonoid
  implicit val stringBuilderMonoid = fj.pre.Monoid.stringBuilderMonoid

  implicit def functionMonoid[A, B](implicit mb: fj.pre.Monoid[B]) = fj.pre.Monoid.functionMonoid[A, B](mb)
  implicit def listMonoid[A] = fj.pre.Monoid.listMonoid[A]
  implicit def optionMonoid[A] = fj.pre.Monoid.optionMonoid[A]
  implicit def streamMonoid[A] = fj.pre.Monoid.streamMonoid[A]
  implicit def arrayMonoid[A] = fj.pre.Monoid.arrayMonoid[A]
}
