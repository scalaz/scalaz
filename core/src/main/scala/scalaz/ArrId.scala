package scalaz

////
/**
 *
 */
////
trait ArrId[=>:[_, _]]  { self =>
  ////
  def id[A]: A =>: A

  ////
  val arrIdSyntax = new scalaz.syntax.ArrIdSyntax[=>:] { def F = ArrId.this }
}

object ArrId {
  @inline def apply[F[_, _]](implicit F: ArrId[F]): ArrId[F] = F

  ////

  ////
}

