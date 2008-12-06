package fjs.pre

object Semigroup {
  implicit val intAdditionSemigroup = fj.pre.Semigroup.intAdditionSemigroup
  implicit val intMultiplicationSemigroup = fj.pre.Semigroup.intMultiplicationSemigroup
  implicit val disjunctionSemigroup = fj.pre.Semigroup.disjunctionSemigroup
  implicit val conjunctionSemigroup = fj.pre.Semigroup.conjunctionSemigroup
  implicit val stringSemigroup = fj.pre.Semigroup.stringSemigroup
  implicit val stringBufferSemigroup = fj.pre.Semigroup.stringBufferSemigroup
  implicit val stringBuilderSemigroup = fj.pre.Semigroup.stringBuilderSemigroup

  implicit def functionSemigroup[A, B](implicit mb: fj.pre.Semigroup[B]) = fj.pre.Semigroup.functionSemigroup[A, B](mb)
  implicit def listSemigroup[A] = fj.pre.Semigroup.listSemigroup[A]
  implicit def nonEmptyListSemigroup[A] = fj.pre.Semigroup.nonEmptyListSemigroup[A]
  implicit def optionSemigroup[A] = fj.pre.Semigroup.optionSemigroup[A]
  implicit def streamSemigroup[A] = fj.pre.Semigroup.streamSemigroup[A]
  implicit def arraySemigroup[A] = fj.pre.Semigroup.arraySemigroup[A]
}
