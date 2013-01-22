package scalaz

////
/**
 * Run a side effect once for each `A` in the `F`.
 */
////
trait Each[F[_]]  { self =>
  ////
  def each[A](fa: F[A])(f: A => Unit)

  // derived functions

  ////
  val eachSyntax = new scalaz.syntax.EachSyntax[F] { def F = Each.this }
}

object Each {
  @inline def apply[F[_]](implicit F: Each[F]): Each[F] = F

  ////

  ////
}
