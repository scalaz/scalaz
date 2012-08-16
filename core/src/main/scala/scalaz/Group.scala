package scalaz

////
/**
 * A group extends [[scalaz.Monoid]] with an inverse element, such that `append(a, inverse(a)) === zero`.
 *
 * References:
 * - [[http://en.wikipedia.org/wiki/Group_(mathematics) Group on Wikipedia]]
 * - [[http://mathworld.wolfram.com/Group.html]]
 *
 * @see [[scalaz.syntax.GroupOps]]
 * @see [[scalaz.Group.GroupLaw]]
 */
////
trait Group[F] extends Monoid[F] { self =>
  ////
  def inverse(f: F): F

  // derived functions

  def minus(f1: F, f2: => F): F = append(f1, inverse(f2))

  trait GroupLaw extends MonoidLaw {
    def inverseExists(a: F)(implicit F: Equal[F]) = F.equal(zero, append(a, inverse(a))) && F.equal(zero, append(inverse(a), a))
  }
  def groupLaw = new GroupLaw {}
  ////
  val groupSyntax = new scalaz.syntax.GroupSyntax[F] { def F = Group.this }
}

object Group {
  @inline def apply[F](implicit F: Group[F]): Group[F] = F

  ////

  ////
}

