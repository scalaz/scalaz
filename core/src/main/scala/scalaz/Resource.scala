package scalaz

import effect._

sealed trait Resource[A] {
  val close: A => IO[Unit]

  import Resource._

  def contramap[B](f: B => A): Resource[B] =
    resource(close compose f)
}

object Resource extends Resources

trait Resources {
  def resource[A](c: A => IO[Unit]): Resource[A] = new Resource[A] {
    val close = c
  }

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
  }
}
