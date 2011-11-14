package scalaz
package effect

// TODO Add to GenTypeClass
sealed trait Resource[A] { self =>
  def close(a: A): IO[Unit]

  import Resource._

  def contramap[B](f: B => A): Resource[B] = new Resource[B] {
    def close(b: B): IO[Unit] = self.close(f(b))
  }
}

object Resource extends Resources

trait Resources {
  implicit val resource = new Contravariant[Resource] {
    def contramap[A, B](r: Resource[A])(f: B => A): Resource[B] = r contramap f
  }

  /*
  TODO move to scalaz.std._
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
}
