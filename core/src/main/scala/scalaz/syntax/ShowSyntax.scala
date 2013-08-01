package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Show` */
sealed abstract class ShowOps[F] extends Ops[F] {
  implicit def F: Show[F]
  ////
  final def show: Cord = F.show(self)
  final def shows: String = F.shows(self)
  final def print: Unit = Console.print(shows)
  final def println: Unit = Console.println(shows)
  ////
}

trait ToShowOps  {
  implicit def ToShowOps[F](v: F)(implicit F0: Show[F]) =
    new ShowOps[F] { def self = v; implicit def F: Show[F] = F0 }

  ////

  ////
}

trait ShowSyntax[F]  {
  implicit def ToShowOps(v: F): ShowOps[F] = new ShowOps[F] { def self = v; implicit def F: Show[F] = ShowSyntax.this.F }
  
  def F: Show[F]
  ////

  ////
}
