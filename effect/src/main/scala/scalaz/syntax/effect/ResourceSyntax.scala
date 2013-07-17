package scalaz
package syntax
package effect

import scalaz.effect.Resource

/** Wraps a value `self` and provides methods related to `Resource` */
sealed abstract class ResourceOps[F] extends Ops[F] {
  implicit def F: Resource[F]
  ////

  def close = F.close(self)
  
  ////
}

trait ToResourceOps  {
  implicit def ToResourceOps[F](v: F)(implicit F0: Resource[F]) =
    new ResourceOps[F] { def self = v; implicit def F: Resource[F] = F0 }

  ////

  ////
}

trait ResourceSyntax[F]  {
  implicit def ToResourceOps(v: F): ResourceOps[F] = new ResourceOps[F] { def self = v; implicit def F: Resource[F] = ResourceSyntax.this.F }
  
  def F: Resource[F]
  ////

  ////
}
