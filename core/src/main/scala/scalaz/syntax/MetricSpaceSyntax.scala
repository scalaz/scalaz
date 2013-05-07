package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MetricSpace` */
@deprecated("MetricSpace is deprecated", "7.0.1")
trait MetricSpaceOps[F] extends Ops[F] {
  implicit def F: MetricSpace[F]
  ////
  final def <===>(a: F): Int = F.distance(self, a)
  ////
}

@deprecated("MetricSpace is deprecated", "7.0.1")
trait ToMetricSpaceOps  {
  implicit def ToMetricSpaceOps[F](v: F)(implicit F0: MetricSpace[F]) =
    new MetricSpaceOps[F] { def self = v; implicit def F: MetricSpace[F] = F0 }

  ////

  ////
}

@deprecated("MetricSpace is deprecated", "7.0.1")
trait MetricSpaceSyntax[F]  {
  implicit def ToMetricSpaceOps(v: F): MetricSpaceOps[F] = new MetricSpaceOps[F] { def self = v; implicit def F: MetricSpace[F] = MetricSpaceSyntax.this.F }
  
  def F: MetricSpace[F]
  ////

  ////
}
