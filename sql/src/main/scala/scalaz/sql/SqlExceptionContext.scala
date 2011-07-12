package scalaz
package sql

import SqlValueT._
import PreparedStatementContext._
import Scalaz._

sealed trait SqlExceptionContext {
  val sqlException: SqlException
  val prepareStatementContext: Option[PreparedStatementContext]
  val query: Option[Query]

  import SqlExceptionContext._

  def setPreparedStatementContext(c: PreparedStatementContext): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = Some(c)
    val query = SqlExceptionContext.this.query
  }

  def unsetPreparedStatementContext: SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = None
    val query = SqlExceptionContext.this.query
  }

  def withPreparedStatementContext(k: PreparedStatementContext => PreparedStatementContext): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = SqlExceptionContext.this.prepareStatementContext map k
    val query = SqlExceptionContext.this.query
  }

  def setQuery(q: Query): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = SqlExceptionContext.this.prepareStatementContext
    val query = Some(q)
  }

  def unsetQuery: SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = SqlExceptionContext.this.prepareStatementContext
    val query = None
  }

  def withQuery(k: Query => Query): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = SqlExceptionContext.this.prepareStatementContext
    val query = SqlExceptionContext.this.query map k
  }

  def setQueryPreparedStatement(q: Query, p: PreparedStatement): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = Some(preparedStatementContext(p))
    val query = Some(q)
  }

  def setQueryPreparedStatementPS(q: Query, p: PreparedStatement, t: JdbcType, i: Int): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = Some(preparedStatementContextPS(t, i)(p))
    val query = Some(q)
  }

  def sqlExceptionStore: SqlExceptionContext |--> SqlException =
    store((prepareStatementContext, query) match {
      case (None, None)       => sqlExceptionContext
      case (Some(c), None)    => sqlExceptionContextPS(c)
      case (None, Some(q))    => sqlExceptionContextQuery(q)
      case (Some(c), Some(q)) => sqlExceptionContextQueryPS(q, c)
    }, sqlException)

  def prepareStatementContextStore: SqlExceptionContext |--> Option[PreparedStatementContext] =
    store((z => (z, query) match {
      case (None, None)       => sqlExceptionContext(sqlException)
      case (Some(c), None)    => sqlExceptionContextPS(c)(sqlException)
      case (None, Some(q))    => sqlExceptionContextQuery(q)(sqlException)
      case (Some(c), Some(q)) => sqlExceptionContextQueryPS(q, c)(sqlException)
    }, prepareStatementContext))

  def queryStore: SqlExceptionContext |--> Option[Query] =
    store((z => (prepareStatementContext, z) match {
      case (None, None)       => sqlExceptionContext(sqlException)
      case (Some(c), None)    => sqlExceptionContextPS(c)(sqlException)
      case (None, Some(q))    => sqlExceptionContextQuery(q)(sqlException)
      case (Some(c), Some(q)) => sqlExceptionContextQueryPS(q, c)(sqlException)
    }, query))

}

object SqlExceptionContext extends SqlExceptionContexts

trait SqlExceptionContexts {
  type SqlException = java.sql.SQLException

  def sqlExceptionContext: SqlException => SqlExceptionContext =
    e => new SqlExceptionContext {
      val sqlException = e
      val prepareStatementContext = None
      val query = None
    }

  def sqlExceptionContextPS(pc: PreparedStatementContext): SqlException => SqlExceptionContext =
    e => new SqlExceptionContext {
      val sqlException = e
      val prepareStatementContext = Some(pc)
      val query = None
    }

  def sqlExceptionContextQuery(q: Query): SqlException => SqlExceptionContext =
    e => new SqlExceptionContext {
      val sqlException = e
      val prepareStatementContext = None
      val query = Some(q)
    }

  def sqlExceptionContextQueryPS(q: Query, pc: PreparedStatementContext): SqlException => SqlExceptionContext =
    e => new SqlExceptionContext {
      val sqlException = e
      val prepareStatementContext = Some(pc)
      val query = Some(q)
    }

  val sqlException: SqlExceptionContext @@ SqlException =
    Lens(_.sqlExceptionStore)

  val prepareStatementContext: SqlExceptionContext @@ Option[PreparedStatementContext] =
    Lens(_.prepareStatementContextStore)

  val query: SqlExceptionContext @@ Option[Query] =
    Lens(_.queryStore)

  implicit def SqlExceptionContextShow: Show[SqlExceptionContext] =
    Show.shows(c =>
     ("""SQL Exception:
      | Exception:
      |    """ + c.sqlException + """
      | SQL:
      |    """ + (c.query match {
          case None => "<no sql>"
          case Some(q) => implicitly[Show[Query]] shows q
        }) + """
      | Prepared Statement:
      |    """ + (c.prepareStatementContext match {
          case None => "<no prepared statement>"
          case Some(s) => implicitly[Show[PreparedStatementContext]] show s
        }) + """
      """).stripMargin)

}
