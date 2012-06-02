package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Show` */
trait ShowOps[F] extends Ops[F] {
  implicit def F: Show[F]
  ////
  final def show: List[Char] = F.show(self)
  final def shows: String = F.show(self).mkString
  final def print: Unit = Console.print(shows)
  final def println: Unit = Console.println(shows)
  final def eprint: Unit = Console.err.print(shows)
  final def eprintln: Unit = Console.err.println(shows)
  ////
}

trait ToShowOps  {
  implicit def ToShowOps[F](v: F)(implicit F0: Show[F]) =
    new ShowOps[F] { def self = v; implicit def F: Show[F] = F0 }

  ////

  ////
}

trait ShowSyntax[F]  {
  implicit def ToShowOps(v: F)(implicit F0: Show[F]): ShowOps[F] = new ShowOps[F] { def self = v; implicit def F: Show[F] = F0 }

  ////

  ////
}
