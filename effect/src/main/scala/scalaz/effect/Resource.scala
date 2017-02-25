package scalaz
package effect

////
import java.io.Closeable

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
  val resourceSyntax = new scalaz.syntax.effect.ResourceSyntax[F] { def F = Resource.this }
}

object Resource {
  @inline def apply[F](implicit F: Resource[F]): Resource[F] = F

  ////

  def resource[A](closeAction: A => IO[Unit]): Resource[A] =
    new Resource[A] {
      def close(a: A): IO[Unit] = closeAction(a)
    }

  def resourceFromCloseable[A <: Closeable]: Resource[A] =
    resource(a => IO(a.close))

  implicit val contravariant: Contravariant[Resource] =
    new Contravariant[Resource] {
      def contramap[A, B](fa: Resource[A])(f: B => A): Resource[B] =
        fa.contramap(f)
    }

  ////
}
