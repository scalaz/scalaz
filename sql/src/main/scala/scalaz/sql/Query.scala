package scalaz
package sql

import Scalaz._

trait Query {
  val sql: String
  val bindings: List[JdbcType]

  import Query._

  def sqlStore: Query |--> String =
    store(s => query(s, bindings), sql)

  def bindingsStore: (Query |--> List[JdbcType]) =
    store(b => query(sql, b), bindings)
}

object Query extends Querys

trait Querys {
  def query(s: String, b: List[JdbcType]): Query = new Query {
    val sql = s
    val bindings = b
  }

  val sql: Query @@ String =
    Lens(_.sqlStore)

  val bindings: Query @@ List[JdbcType] =
    Lens(_.bindingsStore)

  implicit def QueryShow: Show[Query] =
    Show.shows(q => "Query(" + q.sql + ") " + implicitly[Show[List[JdbcType]]].shows(q.bindings))
}
