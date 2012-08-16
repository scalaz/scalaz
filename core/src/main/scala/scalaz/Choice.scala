package scalaz

////
/**
 *
 */
////
trait Choice[=>:[_, _]] extends Category[=>:] { self =>
  ////
  def choice[A, B, C](f: => A =>: C, g: => B =>: C): (A \/ B) =>: C


  // derived functions

  def codiagonal[A]: (A \/ A) =>: A = choice(id, id)

  ////
  val choiceSyntax = new scalaz.syntax.ChoiceSyntax[=>:] { def F = Choice.this }
}

object Choice {
  @inline def apply[F[_, _]](implicit F: Choice[F]): Choice[F] = F

  ////
  ////
}

