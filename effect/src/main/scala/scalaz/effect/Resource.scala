package scalaz
package effect

////
/**
 *
 */
////
trait Resource[F]  { self =>
  ////

  def close(f: F): IO[Unit]

  // derived functions
  def contramap[G](f: G => F): Resource[G] = new Resource[G] {
    def close(g: G): IO[Unit] = self.close(f(g))
  }

  ////
  val resourceSyntax = new scalaz.syntax.effect.ResourceSyntax[F] {}
}

object Resource {
  @inline def apply[F](implicit F: Resource[F]): Resource[F] = F

  ////

  ////
}

