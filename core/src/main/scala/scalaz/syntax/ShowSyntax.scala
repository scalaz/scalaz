package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Show` */
final class ShowOps[F] private[syntax](val self: F)(implicit val F: Show[F]) extends Ops[F] {
  ////
  final def show: Cord = F.show(self)
  final def shows: String = F.shows(self)
  final def print(): Unit = Console.print(shows)
  final def println(): Unit = Console.println(shows)
  ////
}

trait ToShowOps  {
  implicit def ToShowOps[F](v: F)(implicit F0: Show[F]): ShowOps[F] =
    new ShowOps[F](v)

  ////
  implicit final def showInterpolator(sc: StringContext): Show.ShowInterpolator = Show.ShowInterpolator(sc)
  implicit final def cordInterpolator(sc: StringContext): Cord.CordInterpolator = new Cord.CordInterpolator(sc)
  ////
}

trait ShowSyntax[F]  {
  implicit def ToShowOps(v: F): ShowOps[F] = new ShowOps[F](v)(ShowSyntax.this.F)

  def F: Show[F]
  ////

  ////
}
