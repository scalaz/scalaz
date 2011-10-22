package scalaz

trait MetricSpace[F]  { self =>
  ////
  def distance(a: F, b: F): Int

  def contramap[B](f: B => F): MetricSpace[B] = new MetricSpace[B] {
    def distance(a: B, b: B): Int = self.distance(f(a), f(b))
  }

  // derived functions

  ////
  val metricSpaceSyntax = new scalaz.syntax.MetricSpaceSyntax[F] {}
}

object MetricSpace {
  def apply[F](implicit F: MetricSpace[F]): MetricSpace[F] = F

  ////
  val metricSpace = new Contravariant[MetricSpace] {
    def contramap[A, B](r: MetricSpace[A])(f: (B) => A): MetricSpace[B] = r contramap f
  }

  ////
}

