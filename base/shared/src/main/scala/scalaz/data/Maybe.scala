package scalaz
package data

sealed trait MaybeModule extends MaybeFunctions {
  def fold[A, B](ma: Maybe[A])(f: A => B, b: => B): B =
    toOption(ma).fold(b)(f)

  /* typeclass instances */
  def isCovariant: IsCovariant[Maybe]
  def monad: Monad[Maybe]
  def traversable: Traversable[Maybe]
}

private[data] object MaybeImpl extends MaybeModule with MaybeInstances {
  def empty[A]: Maybe[A] = None
  def just[A](a: A): Maybe[A] = Some(a)
  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B = _ match {
    case Some(a) => f(a)
    case None    => n
  }
  def fromOption[A](oa: Option[A]): Maybe[A] = oa
  def toOption[A](ma: Maybe[A]): Option[A] = ma

  def isCovariant: IsCovariant[Maybe] = Scalaz.scalaCovariant[Option]
  def monad: Monad[Maybe] = maybeMonad
  def traversable: Traversable[Maybe] = maybeMonad
}
