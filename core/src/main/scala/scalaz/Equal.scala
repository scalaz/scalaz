package scalaz

////
/**
 *
 */
////
trait Equal[F]  { self =>
  ////
  def equal(a1: F, a2: F): Boolean

  def contramap[G](f: G => F): Equal[G] = new Equal[G] {
    def equal(a1: G, a2: G) = self.equal(f(a1), f(a2))
  }
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

  def equalBy[A, B: Equal](f: A => B): Equal[A] = Equal[B] contramap f


  ////
}

