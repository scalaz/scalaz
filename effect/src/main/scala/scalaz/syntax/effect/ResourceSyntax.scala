package scalaz
package syntax
package effect

import scalaz.effect.Resource

/** Wraps a value `self` and provides methods related to `Resource` */
final class ResourceOps[F] private[syntax](val self: F)(implicit val F: Resource[F]) extends Ops[F] {
  ////

  def close: scalaz.effect.IO[Unit] = F.close(self)

  ////
}

trait ToResourceOps  {
  implicit def ToResourceOps[F](v: F)(implicit F0: Resource[F]): ResourceOps[F] =
    new ResourceOps[F](v)

  ////

  ////
}

trait ResourceSyntax[F]  {
  implicit def ToResourceOps(v: F): ResourceOps[F] = new ResourceOps[F](v)(using ResourceSyntax.this.F)

  def F: Resource[F]
  ////

  ////
}
