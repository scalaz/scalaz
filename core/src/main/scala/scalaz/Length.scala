package scalaz

////
/**
 *
 */
////
@deprecated("Length is deprecated, use Foldable#length instead", "7.1")
trait Length[F[_]]  { self =>
  ////
  def length[A](fa: F[A]): Int
  // derived functions

  ////
  val lengthSyntax = new scalaz.syntax.LengthSyntax[F] { def F = Length.this }
}

@deprecated("Length is deprecated, use Foldable#length instead", "7.1")
object Length {
  @inline def apply[F[_]](implicit F: Length[F]): Length[F] = F

  ////

  ////
}
