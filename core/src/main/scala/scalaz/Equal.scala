package scalaz

////
/**
 *
 */
////
trait Equal[F]  { self =>
  ////
  def equal(a1: F, a2: F): Boolean

  // derived functions

  ////
  val equalSyntax = new scalaz.syntax.EqualSyntax[F] {}
}

object Equal {
  @inline def apply[F](implicit F: Equal[F]): Equal[F] = F

  ////
  /** Creates an Equal instance based on reference equality, `a1 eq a2` */
  def equalA[A <: AnyRef]: Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A): Boolean = a1 eq a2
  }

  def equalBy[A, B](f: A => B)(implicit B: Equal[B]): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = B.equal(f(a1), f(a2))
  }

  ////
}

