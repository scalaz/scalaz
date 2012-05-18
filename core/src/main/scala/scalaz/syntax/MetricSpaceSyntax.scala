package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MetricSpace` */
trait MetricSpaceOps[F] extends Ops[F] {
  implicit def F: MetricSpace[F]
  ////
  final def <===>(a: F): Int = F.distance(self, a)
  ////
}

trait ToMetricSpaceOps  {
  implicit def ToMetricSpaceOps[F](v: F)(implicit F0: MetricSpace[F]) =
    new MetricSpaceOps[F] { def self = v; implicit def F: MetricSpace[F] = F0 }

  ////

  ////
}

trait MetricSpaceSyntax[F]  {
  implicit def ToMetricSpaceOps(v: F)(implicit F0: MetricSpace[F]): MetricSpaceOps[F] = new MetricSpaceOps[F] { def self = v; implicit def F: MetricSpace[F] = F0 }

  ////

  ////
}
