package scalaz

////
/**
 * A type safe alternative to universal equality (`scala.Any#==`).
 *
 * @see [[scalaz.Equal.EqualLaw]]
 */
////
trait Equal[F]  { self =>
  ////
  def equal(a1: F, a2: F): Boolean

  def contramap[G](f: G => F): Equal[G] = new Equal[G] {
    def equal(a1: G, a2: G) = self.equal(f(a1), f(a2))
  }

  /** @return true, if `equal(f1, f2)` is known to be equivalent to `f1 == f2` */
  def equalIsNatural: Boolean = false

  // derived functions

  trait EqualLaw {
    import std.boolean.conditional
    def commutative(f1: F, f2: F): Boolean = equal(f1, f2) == equal(f2, f1)
    def reflexive(f: F): Boolean = equal(f, f)
    def transitive(f1: F, f2: F, f3: F): Boolean = {
      conditional(equal(f1, f2) && equal(f2, f3), equal(f1, f3))
    }
    def naturality(f1: F, f2: F): Boolean = {
      conditional(equalIsNatural, equal(f1, f2) == (f1 == f2))
    }
  }
  def equalLaw = new EqualLaw {}
  ////
  val equalSyntax = new scalaz.syntax.EqualSyntax[F] { def F = Equal.this }
}

object Equal {
  @inline def apply[F](implicit F: Equal[F]): Equal[F] = F

  ////
  /** Creates an Equal instance based on universal equality, `a1 == a2` */
  def equalA[A]: Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A): Boolean = a1 == a2
    override def equalIsNatural: Boolean = true
  }

  /** Creates an Equal instance based on reference equality, `a1 eq a2` */
  def equalRef[A <: AnyRef]: Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A): Boolean = a1 eq a2
  }

  def equalBy[A, B: Equal](f: A => B): Equal[A] = Equal[B] contramap f

  def equalContravariant: Contravariant[Equal] = new Contravariant[Equal] {
    def contramap[A, B](r: Equal[A])(f: B => A) = r.contramap(f)
  }

  def equal[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = f(a1, a2)
  }

  ////
}
