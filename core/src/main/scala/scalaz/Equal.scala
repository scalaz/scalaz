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

  trait EqualLaw {
    def commutative(f1: F, f2: F): Boolean = equal(f1, f2) == equal(f2, f2)
    def reflexive(f: F): Boolean = equal(f, f)
    def transitive(f1: F, f2: F, f3: F): Boolean = (equal(f1, f2) && equal(f2, f3)) == equal(f1, f3)
  }
  def equalLaw = new EqualLaw {}
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

  def equalContravariant: Contravariant[Equal] = new Contravariant[Equal] {
    def contramap[A, B](r: Equal[A])(f: (B) => A) = r.contramap(f)
  }
  ////
}

