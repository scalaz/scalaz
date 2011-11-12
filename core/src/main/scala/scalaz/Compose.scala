package scalaz

////
/**
 *
 */
////
trait Compose[=>:[_, _]]  { self =>
  ////
  def compose[A, B, C](f: B =>: C, g: A =>: B): (A =>: C)

  ////
  val composeSyntax = new scalaz.syntax.ComposeSyntax[=>:] {}
}

object Compose {
  @inline def apply[F[_, _]](implicit F: Compose[F]): Compose[F] = F

  ////
  //  TODO
  //  implicit def LensCompose: Compose[Lens] = new Compose[Lens] {
  //    def compose[A, B, C](f: Lens[B, C], g: Lens[A, B]) =
  //      f >=> g
  //  }
  ////
}

