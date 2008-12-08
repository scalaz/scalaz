package scalaz.database

import control.Semigroup
import control.Semigroup.semigroup
import control.SemigroupW._
import java.sql.{Connection, Savepoint}

import java.util.{Map, Properties}

sealed trait Database[+A] {
  def apply(c: Connection): A

  import Database._

  def map[B](f: A => B): Database[B] = Function1Database(f compose (apply(_)))

  def flatMap[B](f: A => Database[B]): Database[B] = Function1Database(c => f(apply(c))(c))

  def unary_~ = Function1Database(c => {
    apply(c)
    ()
  })
}

object Database {
  implicit def Function1Database[A](f: Connection => A) = new Database[A] {
    def apply(c: Connection) = f(c)
  }

  implicit def DatabaseFunction1[A](d: Database[A]) = d(_: Connection)

  implicit def DatabaseSemigroup[A](implicit s: Semigroup[A]) = semigroup[Database[A]](d1 => d2 => Function1Database(c => d1.apply(c) |+| d2.apply(c)))

  def constant[A](a: A) = Function1Database(c => a)

  def id = Function1Database(c => c)

  def clearWarnings = Function1Database(_.clearWarnings)

  def close = Function1Database(_.close)

  def commit = Function1Database(_.commit)

  def arrayOf(name: String, elements: List[AnyRef]) = Function1Database(_.createArrayOf(name, elements.toArray))

  def blob = Function1Database(_.createBlob)

  def clob = Function1Database(_.createClob)

  def nclob = Function1Database(_.createNClob)

  def sqlxml = Function1Database(_.createSQLXML)

  def statement = Function1Database(_.createStatement)

  def statement(t: ResultSetType, c: ResultSetConcurrencyType) = Function1Database(_.createStatement(t.asInt, c.asInt))

  def statement(t: ResultSetType, c: ResultSetConcurrencyType, h: ResultSetHoldabilityType) = Function1Database(_.createStatement(t.asInt, c.asInt, h.asInt))

  def struct(name: String, attributes: List[AnyRef]) = Function1Database(_.createStruct(name, attributes.toArray))

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

  def prepareStatementIndices(sql: String, columnIndices: List[Int]) = Function1Database(_.prepareStatement(sql, columnIndices.toArray))

  def prepareStatement(sql: String, t: ResultSetType, c: ResultSetConcurrencyType) = Function1Database(_.prepareStatement(sql, t.asInt, c.asInt))

  def prepareStatement(sql: String, t: ResultSetType, c: ResultSetConcurrencyType, h: ResultSetHoldabilityType) = Function1Database(_.prepareStatement(sql, t.asInt, c.asInt, h.asInt))

  def prepareStatement(sql: String, columnNames: List[String]) = Function1Database(_.prepareStatement(sql, columnNames.toArray))

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

  def updateIndices(sql: String, columnIndices: List[Int]) = Function1Database(_.createStatement.executeUpdate(sql, columnIndices.toArray))

  def update(sql: String, columnNames: List[String]) = Function1Database(_.createStatement.executeUpdate(sql, columnNames.toArray))

  def execute(sql: String) = Function1Database(_.createStatement.execute(sql))

  def execute(sql: String, g: KeyGeneration) = Function1Database(_.createStatement.execute(sql, g.asInt))

  def executeIndices(sql: String, columnIndices: List[Int]) = Function1Database(_.createStatement.execute(sql, columnIndices.toArray))

  def execute(sql: String, columnNames: List[String]) = Function1Database(_.createStatement.execute(sql, columnNames.toArray))
}
