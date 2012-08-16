package scalaz

////
/**
 *
 */
////
trait Copointed[F[_]] extends Functor[F] { self =>
  ////
  /** Also known as `extract` / `copure` */
  def copoint[A](p: F[A]): A

  // derived functions

  /** alias for `copoint` */
  def copure[A](p: F[A]): A = copoint(p)

  ////
  val copointedSyntax = new scalaz.syntax.CopointedSyntax[F] { def F = Copointed.this }
}

object Copointed {
  @inline def apply[F[_]](implicit F: Copointed[F]): Copointed[F] = F

  ////

  ////
}

