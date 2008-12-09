// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database

import java.sql.{Connection, SQLException, DriverManager => DM}

/**
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
  def connector(f: => Either[SQLException, Connection]) = new Connector {
    def connect = f
  }

  def connectionConnector(c: => Connection) = connector(Right(c))

  def exceptionConnector(e: => SQLException) = connector(Left(e))

  def driverManager(url: String) = connector(
    try {
      Right(DM.getConnection(url))
    } catch {
      case e: SQLException => Left(e)
    })

  def driverManager(url: String, username: String, password: String) = connector(
    try {
      Right(DM.getConnection(url, username, password))
    } catch {
      case e: SQLException => Left(e)
    })
}
