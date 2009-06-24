// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.{Connection, SQLException, DriverManager => DM}

/**
 * A value that is either a successful database connection or is a failed attempt for a database connection.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Connector {
  def connect: Either[SQLException, Connection]
}

/**
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Connector {
  /**
   * Construct a connector with the given disjunctive value.
   */
  def connector(f: => Either[SQLException, Connection]) = new Connector {
    def connect = f
  }

  /**
   * Construct a connector that is a successful database connection.
   */
  def connectionConnector(c: => Connection) = connector(Right(c))

  /**
   * Construct a connector that is a failed database connection.
   */
  def exceptionConnector(e: => SQLException) = connector(Left(e))

  /**
   * Construct a connector from the given url using a JDBC DriverManager.
   */
  def driverManager(url: String) = connector(
    try {
      Right(DM.getConnection(url))
    } catch {
      case e: SQLException => Left(e)
    })

  /**
   * Construct a connector from the given url, username and password using a JDBC DriverManager.
   */
  def driverManager(url: String, username: String, password: String) = connector(
    try {
      Right(DM.getConnection(url, username, password))
    } catch {
      case e: SQLException => Left(e)
    })
}
