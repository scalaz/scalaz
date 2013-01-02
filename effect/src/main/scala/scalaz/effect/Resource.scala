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
  val resourceSyntax = new scalaz.syntax.effect.ResourceSyntax[F] { def F = Resource.this }
}

object Resource {
  @inline def apply[F](implicit F: Resource[F]): Resource[F] = F

  ////
  /*
    TODO move to scalaz.effect.std._. Cumbersome, I know.
    import java.io.{OutputStream, InputStream}
    import java.sql.{PreparedStatement, ResultSet, Statement, Connection}

    implicit val InputStreamResource: Resource[InputStream] = new Resource[InputStream] {
      val close = (r: InputStream) => IO(r.close)
    }

    implicit val OutputStreamResource: Resource[OutputStream] = new Resource[OutputStream] {
      val close = (r: OutputStream) => IO(r.close)
    }

    implicit val SQLConnectionResource: Resource[Connection] = new Resource[Connection] {
      val close = (r: Connection) => IO(r.close)
    }

    implicit val SQLStatementResource: Resource[Statement] = new Resource[Statement] {
      val close = (r: Statement) => IO(r.close)
    }

    implicit val SQLPreparedStatementResource: Resource[PreparedStatement] = new Resource[PreparedStatement] {
      val close = (r: PreparedStatement) => IO(r.close)
    }

    implicit val SQLResultSetResource: Resource[ResultSet] = new Resource[ResultSet] {
      val close = (r: ResultSet) => IO(r.close)
    }*/
  ////
}
