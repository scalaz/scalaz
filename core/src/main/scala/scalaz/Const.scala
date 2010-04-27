package scalaz

/**
 * Used to generate Phantom Applicative Functors and categories from a Monoidal type A
 *
 * @see scalaz.Applicative#MonoidalApplicative
 * @see scalaz.Category#MonoidCategory
 */
case class Const[A, +B](value: A) extends NewType[A]
case class Const2[A, +B, +C](value: A) extends NewType[A]
