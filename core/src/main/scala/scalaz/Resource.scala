package scalaz

import java.sql.{ResultSet, Statement, Connection}
import java.io.{OutputStream, InputStream}

sealed trait Resource[T] {
  def close(t: T): Unit
}

object Resource {
  implicit val ResourceContravariant: Contravariant[Resource] = new Contravariant[Resource] {
    def contramap[A, B](r: Resource[A], f: B => A) = new Resource[B] {
      def close(b: B) = r close (f(b))
    }
  }

  implicit val InputStreamResource: Resource[InputStream] = new Resource[InputStream] {
    def close(c: InputStream) = c.close
  }

  implicit val OutputStreamResource: Resource[OutputStream] = new Resource[OutputStream] {
    def close(c: OutputStream) = c.close
  }

  implicit val SQLConnectionResource: Resource[Connection] = new Resource[Connection] {
    def close(c: Connection) = c.close
  }

  implicit val SQLStatementResource: Resource[Statement] = new Resource[Statement] {
    def close(c: Statement) = c.close
  }

  implicit val SQLResultSetResource: Resource[ResultSet] = new Resource[ResultSet] {
    def close(c: ResultSet) = c.close
  }
}

trait Resources {
  def resource[T](cl: T => Unit): Resource[T] = new Resource[T] {
    def close(t: T) = cl(t)
  }

  def withResource[T, R](
                          value: => T
                        , evaluate: T => R
                        , whenComputing: Throwable => R = (t: Throwable) => throw t
                        , whenClosing: Throwable => Unit = _ => ()
                        )(implicit r: Resource[T]): R =
    try {
      val u = value
      try {
        evaluate(u)
      } finally {
        try {
          r close u
        } catch {
          case ex => whenClosing(ex)
        }
      }
    } catch {
      case ex => whenComputing(ex)
    }
}
