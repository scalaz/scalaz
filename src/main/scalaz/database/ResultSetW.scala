package scalaz.database, but 

import java.sql.ResultSet

sealed trait ResultSetW {
  val resultset: ResultSet

  def ~>[T](f: ImmutableResultSet => T) = new Iterator[T] {
    var h = resultset.next
    def next = {
      val t = f(resultset)
      h = resultset.next
      t
    }
    def hasNext = h
  }
}

object ResultSetW {
  implicit def resultset(r: ResultSet) = new ResultSetW {
    val resultset = r
  }
}
