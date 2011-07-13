package scalaz
package sql

import Scalaz._

sealed trait PreparedStatementContext {
  val preparedStatement: PreparedStatement
  val parameter: Option[(JdbcType, Int)]

  import PreparedStatementContext._

  def setParameter(t: JdbcType, n: Int) =
    preparedStatementContextPS(t, n)(preparedStatement)

  def unsetParameter =
    preparedStatementContext(preparedStatement)

  def withParameter(k: (JdbcType, Int) => (JdbcType, Int)) =
    parameter match {
      case None         => this
      case Some((p, q)) => k(p, q) match { case (x, y) => setParameter(x, y) }
    }

  def withType(k: JdbcType => JdbcType) =
    withParameter { case (t, i) => (k(t), i) }

  def withIndex(k: Int => Int) =
    withParameter { case (t, i) => (t, k(i)) }

  def parameterOr(o: => (JdbcType, Int)): (JdbcType, Int) =
    parameter getOrElse o

  def parameterType: Option[JdbcType] =
    parameter map (_._1)

  def parameterTypeOr(t: => JdbcType): JdbcType =
    parameterType getOrElse t

  def parameterIndex: Option[Int] =
    parameter map (_._2)

  def parameterIndexOr(n: => Int): Int =
    parameterIndex getOrElse n

  def preparedStatementStore: PreparedStatementContext |--> PreparedStatement =
    store(parameter match {
      case None => preparedStatementContext
      case Some((t, i)) => preparedStatementContextPS(t, i)
    }, preparedStatement)

  def parameterStore: PreparedStatementContext |--> Option[(JdbcType, Int)] =
    store((p => (p match {
      case None => preparedStatementContext
      case Some((t, i)) => preparedStatementContextPS(t, i)
    })(preparedStatement), parameter))

}

object PreparedStatementContext extends PreparedStatementContexts

trait PreparedStatementContexts {

  def preparedStatementContext: PreparedStatement => PreparedStatementContext =
    e => new PreparedStatementContext {
      val preparedStatement = e
      val parameter = None
    }

  def preparedStatementContextPS(t: JdbcType, i: Int): PreparedStatement => PreparedStatementContext =
    e => new PreparedStatementContext {
      val preparedStatement = e
      val parameter = Some(t, i)
    }

  val preparedStatement: PreparedStatementContext @@ PreparedStatement =
    Lens(_.preparedStatementStore)

  val parameter: PreparedStatementContext @@ Option[(JdbcType, Int)] =
    Lens(_.parameterStore)

  implicit def PreparedStatementContextShow: Show[PreparedStatementContext] =
    Show.shows(q => "PreparedStatementContext[ " + q.preparedStatement + " " +
      (q.parameter match {
         case None => "<no parameter>"
         case Some((t, i)) => "with parameter {" + implicitly[Show[JdbcType]].shows(t) + ", " + implicitly[Show[Int]].show(i) +"}"
      }) + " ]")

}