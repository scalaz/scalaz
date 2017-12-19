package scalaz
package data

sealed trait MaybeModule extends MaybeFunctions {
  def fold[A, B](ma: Maybe[A])(f: A => B, b: => B): B =
    toOption(ma).fold(b)(f)
}

private[scalaz] object MaybeImpl extends MaybeModule {
  def empty[A]: Maybe[A] = None
  def just[A](a: A): Maybe[A] = Some(a)
  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B = _ match {
    case Some(a) => f(a)
    case None    => n
  }
  def fromOption[A](oa: Option[A]): Maybe[A] = oa
  def toOption[A](ma: Maybe[A]): Option[A] = ma

  object Just {
    def unapply[A](ma: Maybe[A]): Option[A] = toOption(ma)
  }

  object Empty {
    def unapply[A](ma: Maybe[A]): Boolean = toOption(ma).isEmpty
  }
}
