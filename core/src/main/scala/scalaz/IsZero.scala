package scalaz

sealed trait IsZero[A] {
  val isZero: A => Boolean

  def apply(a: A) = isZero(a)

  def isNotZero: A => Boolean =
    a => !isZero(a)

  def contramap[B](f: B => A): IsZero[B] =
    IsZero.isZero(isZero compose f)
}

object IsZero extends IsZeros

trait IsZeros {
  def isZero[A](f: A => Boolean): IsZero[A] =
    new IsZero[A] {
      val isZero = f
    }

  def eqIsZero[A](a: A)(implicit e: Equal[A]): IsZero[A] =
    isZero(e.equal(a))

  implicit def EqualZeroIsZero[A](implicit e: Equal[A], z: Zero[A]): IsZero[A] =
    eqIsZero(z.zero)
}
