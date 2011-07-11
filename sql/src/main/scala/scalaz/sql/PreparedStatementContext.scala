package scalaz
package sql

import Scalaz._

sealed trait PreparedStatementContext {
  val preparedStatement: PreparedStatement
  val parameters: Option[(JdbcType, Int)]

  import PreparedStatementContext._

  def setParameters(t: JdbcType, n: Int) =
    preparedStatementContextPS(t, n)(preparedStatement)

  def unsetParameters =
    preparedStatementContext(preparedStatement)

  def withParameters(k: (JdbcType, Int) => (JdbcType, Int)) =
    parameters match {
      case None         => this
      case Some((p, q)) => k(p, q) match { case (x, y) => setParameters(x, y) }
    }

  def parametersOr(o: => (JdbcType, Int)): (JdbcType, Int) =
    parameters getOrElse o

  def parametersType: Option[JdbcType] =
    parameters map (_._1)

  def parametersTypeOr(t: => JdbcType): JdbcType =
    parametersType getOrElse t

  def parametersIndex: Option[Int] =
    parameters map (_._2)

  def parametersIndexOr(n: => Int): Int =
    parametersIndex getOrElse n

  def preparedStatementStore: PreparedStatementContext |--> PreparedStatement =
    store(parameters match {
      case None => preparedStatementContext
      case Some((t, i)) => preparedStatementContextPS(t, i)
    }, preparedStatement)

  def parametersStore: PreparedStatementContext |--> Option[(JdbcType, Int)] =
    store((p => (p match {
      case None => preparedStatementContext
      case Some((t, i)) => preparedStatementContextPS(t, i)
    })(preparedStatement), parameters))

}

object PreparedStatementContext extends PreparedStatementContexts

trait PreparedStatementContexts {

  def preparedStatementContext: PreparedStatement => PreparedStatementContext =
    e => new PreparedStatementContext {
      val preparedStatement = e
      val parameters = None
    }

  def preparedStatementContextPS(t: JdbcType, i: Int): PreparedStatement => PreparedStatementContext =
    e => new PreparedStatementContext {
      val preparedStatement = e
      val parameters = Some(t, i)
    }

  val preparedStatement: PreparedStatementContext @@ PreparedStatement =
    Lens(_.preparedStatementStore)

  val parameters: PreparedStatementContext @@ Option[(JdbcType, Int)] =
    Lens(_.parametersStore)

}