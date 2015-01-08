package scalaz

////
/**
 *
 */
////
trait Associative[=>:[_, _]]  { self =>
  ////

  def reassociateLeft[A, B, C](f: A =>: (B =>: C)): (A =>: B) =>: C

  def reassociateRight[A, B, C](f: (A =>: B) =>: C): A =>: (B =>: C)

  ////
  val associativeSyntax = new scalaz.syntax.AssociativeSyntax[=>:] { def F = Associative.this }
}

object Associative {
  @inline def apply[F[_, _]](implicit F: Associative[F]): Associative[F] = F

  ////

  ////
}
