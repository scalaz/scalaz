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

  import Isomorphism._

  def fromIso[F, G](D: F <=> G)(implicit M: Resource[G]): Resource[F] =
    new IsomorphismResource[F, G] {
      override def G: Resource[G] = M
      override def iso: F <=> G = D
    }

  ////

  def resource[A](closeAction: A => IO[Unit]): Resource[A] =
    new Resource[A] {
      def close(a: A): IO[Unit] = closeAction(a)
    }

  def resourceFromAutoCloseable[A <: java.lang.AutoCloseable]: Resource[A] =
    resource(a => IO(a.close()))

  def resourceFromCloseable[A <: Closeable]: Resource[A] =
    resource(a => IO(a.close))

  implicit val contravariant: Contravariant[Resource] =
    new Contravariant[Resource] {
      def contramap[A, B](fa: Resource[A])(f: B => A): Resource[B] =
        fa.contramap(f)
    }

  ////
}

trait IsomorphismResource[F, G] extends Resource[F] {
  implicit def G: Resource[G]
  ////
  import Isomorphism._

  def iso: F <=> G

  override def close(f: F): IO[Unit] =
    G.close(iso.to(f))
  ////
}
