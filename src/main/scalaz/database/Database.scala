// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import sql.Relation
import control.Semigroup
import control.Semigroup.semigroup
import control.SemigroupW._
import java.sql.{Connection, Savepoint}

import java.util.{Map, Properties}

/**
 * A functor over a database connection.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Database[+A] {
  /**
   * Returns a value for the given database connection.
   */
  def apply(c: Connection): A

  import Database._

  /**
   * Maps the given function across this database connection functor.
   */
  def map[B](f: A => B): Database[B] = Function1Database(f compose (apply(_)))

  /**
   * Binds the given function across this database connection functor.
   */
  def flatMap[B](f: A => Database[B]): Database[B] = Function1Database(c => f(apply(c))(c))

  /**
   * Returns a database connection functor that executes this database connection functor but ignores the result.
   */
  def unary_~ = Function1Database(c => {
    apply(c)
    ()
  })
}

/**
 * Functions over database connection functors.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Database {
  /**
   * Converts the given function into a database connection functor.
   */
  implicit def Function1Database[A](f: Connection => A) = new Database[A] {
    def apply(c: Connection) = f(c)
  }

  /**
   * Returns a function for the given database connection functor.
   */
  implicit def DatabaseFunction1[A](d: Database[A]) = d(_: Connection)

  /**
   * An associative operation for a database connection functor.
   */
  implicit def DatabaseSemigroup[A](implicit s: Semigroup[A]) = semigroup[Database[A]](d1 => d2 => Function1Database(c => d1.apply(c) |+| d2.apply(c)))

  /**
   * Constructs a database functor that always produces the given value (the unital operation for the database connection monad).
   */
  def constant[A](a: A) = Function1Database(c => a)

  /**
   * Returns an indentity database connection functor.
   */
  def id = Function1Database(c => c)

  def clearWarnings = Function1Database(_.clearWarnings)

  def close = Function1Database(_.close)

  def commit = Function1Database(_.commit)

  def arrayOf(name: String, elements: AnyRef*) = Function1Database(_.createArrayOf(name, elements.toArray))

  def blob = Function1Database(_.createBlob)

  def clob = Function1Database(_.createClob)

  def nclob = Function1Database(_.createNClob)

  def sqlxml = Function1Database(_.createSQLXML)

  def statement = Function1Database(_.createStatement)

  def statement(t: ResultSetType, c: ResultSetConcurrencyType) = Function1Database(_.createStatement(t.asInt, c.asInt))

  def statement(t: ResultSetType, c: ResultSetConcurrencyType, h: ResultSetHoldabilityType) = Function1Database(_.createStatement(t.asInt, c.asInt, h.asInt))

  def struct(name: String, attributes: AnyRef*) = Function1Database(_.createStruct(name, attributes.toArray))

  def autoCommit = Function1Database(_.getAutoCommit)

  def catalog = Function1Database(_.getCatalog)

  def clientInfo = Function1Database(_.getClientInfo)

  def clientInfo(name: String) = Function1Database(_.getClientInfo(name))

  def holdability = Function1Database(c => ResultSetHoldabilityType.fromInt(c.getHoldability).get)

  def metadata = Function1Database(_.getMetaData)

  def transactionIsolation = Function1Database(c => TransactionIsolation.fromInt(c.getTransactionIsolation).get)

  def typeMap = Function1Database(_.getTypeMap)

  def warnings = Function1Database(_.getWarnings)

  def closed = Function1Database(_.isClosed)

  def readonly = Function1Database(_.isReadOnly)

  def valid(timeout: Int) = Function1Database(_.isValid(timeout))

  def nativeSQL(sql: String) = Function1Database(_.nativeSQL(sql))

  def prepareCall(sql: String) = Function1Database(_.prepareCall(sql))

  def prepareCall(sql: String, t: ResultSetType, c: ResultSetConcurrencyType) = Function1Database(_.prepareCall(sql, t.asInt, c.asInt))

  def prepareCall(sql: String, t: ResultSetType, c: ResultSetConcurrencyType, h: ResultSetHoldabilityType) = Function1Database(_.prepareCall(sql, t.asInt, c.asInt, h.asInt))

  def prepareStatement(sql: String) = Function1Database(_.prepareStatement(sql))

  def prepareStatement(sql: String, g: KeyGeneration) = Function1Database(_.prepareStatement(sql, g.asInt))

  def prepareStatementIndices(sql: String, columnIndices: Int*) = Function1Database(_.prepareStatement(sql, columnIndices.toArray))

  def prepareStatement(sql: String, t: ResultSetType, c: ResultSetConcurrencyType) = Function1Database(_.prepareStatement(sql, t.asInt, c.asInt))

  def prepareStatement(sql: String, t: ResultSetType, c: ResultSetConcurrencyType, h: ResultSetHoldabilityType) = Function1Database(_.prepareStatement(sql, t.asInt, c.asInt, h.asInt))

  def prepareStatement(sql: String, columnNames: String*) = Function1Database(_.prepareStatement(sql, columnNames.toArray))

  def releaseSavepoint(s: Savepoint) = Function1Database(_.releaseSavepoint(s))

  def rollback = Function1Database(_.rollback)

  def rollback(s: Savepoint) = Function1Database(_.rollback(s))

  def autoCommit(c: Boolean) = Function1Database(_.setAutoCommit(c))

  def catalog(catalog: String) = Function1Database(_.setCatalog(catalog))

  def clientInfo(properties: Properties) = Function1Database(_.setClientInfo(properties))

  def holdability(h: ResultSetHoldabilityType) = Function1Database(_.setHoldability(h.asInt))

  def readOnly(c: Boolean) = Function1Database(_.setReadOnly(c))

  def savepoint = Function1Database(_.setSavepoint)

  def savepoint(name: String) = Function1Database(_.setSavepoint(name))

  def transactionIsolation(i: TransactionIsolation) = Function1Database(_.setTransactionIsolation(i.asInt))

  def typeMap(map: Map[String, Class[_]]) = Function1Database(_.setTypeMap(map))

  def query(sql: String) = Function1Database(_.createStatement.executeQuery(sql))

  def update(sql: String) = Function1Database(_.createStatement.executeUpdate(sql))

  def update(sql: String, g: KeyGeneration) = Function1Database(_.createStatement.executeUpdate(sql, g.asInt))

  def updateIndices(sql: String, columnIndices: Int*) = Function1Database(_.createStatement.executeUpdate(sql, columnIndices.toArray))

  def update(sql: String, columnNames: String*) = Function1Database(_.createStatement.executeUpdate(sql, columnNames.toArray))

  def execute(sql: String) = Function1Database(_.createStatement.execute(sql))

  def execute(sql: String, g: KeyGeneration) = Function1Database(_.createStatement.execute(sql, g.asInt))

  def executeIndices(sql: String, columnIndices: Int*) = Function1Database(_.createStatement.execute(sql, columnIndices.toArray))

  def execute(sql: String, columnNames: String*) = Function1Database(_.createStatement.execute(sql, columnNames.toArray))

  def from(relation: Relation, relations: Relation*) = sql.From.from(relation :: relations.toList)
}
