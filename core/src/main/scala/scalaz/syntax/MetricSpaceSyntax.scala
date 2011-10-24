package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MetricSpace` */
trait MetricSpaceV[F] extends SyntaxV[F] {
  implicit def F: MetricSpace[F]
  ////
  def <===>(a: F): Int = F.distance(self, a)
  ////
}

trait ToMetricSpaceSyntax  {
  implicit def ToMetricSpaceV[F](v: F)(implicit F0: MetricSpace[F]) =
    new MetricSpaceV[F] { def self = v; implicit def F: MetricSpace[F] = F0 }

  ////

  ////
}

trait MetricSpaceSyntax[F]  {
  implicit def ToMetricSpaceV(v: F)(implicit F0: MetricSpace[F]): MetricSpaceV[F] = new MetricSpaceV[F] { def self = v; implicit def F: MetricSpace[F] = F0 }

  ////

  ////
}
