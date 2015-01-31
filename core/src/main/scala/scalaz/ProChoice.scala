package scalaz

////
/**
 * Strength on a coproduct.
 */
////
trait ProChoice[=>:[_, _]] extends Profunctor[=>:] { self =>
  ////
  def left[A, B, C](fa: A =>: B): (A \/ C) =>: (B \/ C)

  def right[A, B, C](fa: A =>: B): (C \/ A) =>: (C \/ B)

  ////
  val proChoiceSyntax = new scalaz.syntax.ProChoiceSyntax[=>:] { def F = ProChoice.this }
}

object ProChoice {
  @inline def apply[F[_, _]](implicit F: ProChoice[F]): ProChoice[F] = F

  ////

  ////
}
