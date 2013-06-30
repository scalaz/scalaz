package scalaz

////
/**
 * Run a side effect once for each `A` in the `F`.
 */
////
@deprecated("Each is deprecated", "7.1")
trait Each[F[_]]  { self =>
  ////
  def each[A](fa: F[A])(f: A => Unit)

  // derived functions

  ////
  val eachSyntax = new scalaz.syntax.EachSyntax[F] { def F = Each.this }
}

@deprecated("Each is deprecated", "7.1")
object Each {
  @inline def apply[F[_]](implicit F: Each[F]): Each[F] = F

  ////

  ////
}
