package scalaz

////
/**
 * Generalizing downstar of a costrong Functor
 */
////
trait Costrong[=>:[_, _]] extends Profunctor[=>:] { self =>
  ////
  def left[A, B, C](fa: A =>: B): (A \/ C) =>: (B \/ C)

  def right[A, B, C](fa: A =>: B): (C \/ A) =>: (C \/ B)

  ////
  val costrongSyntax = new scalaz.syntax.CostrongSyntax[=>:] { def F = Costrong.this }
}

object Costrong {
  @inline def apply[F[_, _]](implicit F: Costrong[F]): Costrong[F] = F

  ////

  ////
}
