package scalaz
package syntax
package effect

import scalaz.effect.Resource

/** Wraps a value `self` and provides methods related to `Resource` */
trait ResourceV[F] extends SyntaxV[F] {
  implicit def F: Resource[F]
  ////

  def close = F.close(self)
  
  ////
}

trait ToResourceV  {
  implicit def ToResourceV[F](v: F)(implicit F0: Resource[F]) =
    new ResourceV[F] { def self = v; implicit def F: Resource[F] = F0 }

  ////

  ////
}

trait ResourceSyntax[F]  {
  implicit def ToResourceV(v: F)(implicit F0: Resource[F]): ResourceV[F] = new ResourceV[F] { def self = v; implicit def F: Resource[F] = F0 }

  ////

  ////
}
