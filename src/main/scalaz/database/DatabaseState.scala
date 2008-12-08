package scalaz.database

import java.sql.SQLException

sealed trait DatabaseState {
  val con: Connector
  val terminal: Database[Unit]

  def apply[A](db: Database[A]): Either[SQLException, A] =
    con.connect.right.flatMap(c =>
      try {
        c setAutoCommit false
        val a = db(c)
        terminal(c)
        Right(a)
      } catch {
        case e: SQLException => {
          c.rollback
          Left(e)
        }
      } finally {
        c.close
      })
}

object DatabaseState {
  def databaseState(c: Connector, t: Database[Unit]) = new DatabaseState {
    val con = c
    val terminal = t
  }

  import Connector._

  def reader(c: Connector): DatabaseState = databaseState(c, Database.rollback)

  def reader(url: String): DatabaseState = reader(driverManager(url))

  def reader(url: String, username: String, password: String): DatabaseState =
    reader(driverManager(url, username, password))

  def writer(c: Connector): DatabaseState = databaseState(c, Database.commit)

  def writer(url: String): DatabaseState = writer(driverManager(url))

  def writer(url: String, username: String, password: String): DatabaseState =
    writer(driverManager(url, username, password))
}
