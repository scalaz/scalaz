package scalaz

////
/**
 *
 */
////
trait Group[F] extends Monoid[F] { self =>
  ////
  def inverse(f: F): F

  // derived functions
  trait GroupLaw extends MonoidLaw {
    def inverseExists(a: F)(implicit F: Equal[F]) = F.equal(zero, append(a, inverse(a))) && F.equal(zero, append(inverse(a), a))
  }
  def groupLaw = new GroupLaw {}
  ////
  val groupSyntax = new scalaz.syntax.GroupSyntax[F] {}
}

object Group {
  @inline def apply[F](implicit F: Group[F]): Group[F] = F

  ////

  ////
}

