package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MetricSpace` */
trait MetricSpaceV[F] extends SyntaxV[F] {
  ////
  def <===>(a: F)(implicit m: MetricSpace[F]): Int = m distance(self, a)
  ////
}

trait ToMetricSpaceSyntax {
  implicit def ToMetricSpaceV[F](v: F) =
    new MetricSpaceV[F] {
      def self = v
    }

  ////

  ////
}

trait MetricSpaceSyntax[F] {
  implicit def ToMetricSpaceV(v: F): MetricSpaceV[F] = new MetricSpaceV[F] {
    def self = v
  }

  ////

  ////
}
