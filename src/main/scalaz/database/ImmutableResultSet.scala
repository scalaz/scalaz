// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.database


import java.sql.{ResultSet, Array, Blob, Clob, Date, ResultSetMetaData, NClob, Ref, RowId, SQLXML, Time, Timestamp, SQLWarning}
import java.io.{InputStream, Reader}
import java.util.{Calendar, Map}
import java.math.BigDecimal
import java.net.URL

/**
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait ImmutableResultSet {
  def findColumn(column: String): Int
  def array(column: String): Array
  def array(index: Int): Array
  def asciiStream(column: String): InputStream
  def asciiStream(index: Int): InputStream
  def bigDecimal(column: String): BigDecimal
  def bigDecimal(index: Int): BigDecimal
  def binaryStream(column: String): InputStream
  def binaryStream(index: Int): InputStream
  def blob(column: String): Blob
  def blob(index: Int): Blob
  def boolean(column: String): Boolean
  def boolean(index: Int): Boolean
  def byte(column: String): Byte
  def byte(index: Int): Byte
  def bytes(column: String): scala.Array[Byte]
  def bytes(index: Int): scala.Array[Byte]
  def characterStream(column: String): Reader
  def characterStream(index: Int): Reader
  def clob(column: String): Clob
  def clob(index: Int): Clob
  def concurrency: ResultSetConcurrencyType
  def cursor: String
  def date(column: String): Date
  def date(column: String, cal: Calendar): Date
  def date(index: Int): Date
  def date(index: Int, cal: Calendar): Date
  def double(column: String): Double
  def double(index: Int): Double
  def fetchDirection: ResultSetFetchDirection
  def fetchSize: Int
  def float(column: String): Float
  def float(index: Int): Float
  def holdability: ResultSetHoldabilityType
  def int(column: String): Int
  def int(index: Int): Int
  def long(column: String): Long
  def long(index: Int): Long
  def metadata: ResultSetMetaData
  def ncharacterStream(column: String): Reader
  def ncharacterStream(index: Int): Reader
  def nclob(column: String): NClob
  def nclob(index: Int): NClob
  def nstring(column: String): String
  def nstring(index: Int): String
  def value(column: String): AnyRef
  def value(column: String, m: Map[String, Class[_]]): AnyRef
  def value(index: Int): AnyRef
  def value(index: Int, m: Map[String, Class[_]]): AnyRef
  def ref(column: String): Ref
  def ref(index: Int): Ref
  def row: Int
  def rowId(column: String): RowId
  def rowId(index: Int): RowId
  def short(column: String): Short
  def short(index: Int): Short
  def sqlxml(column: String): SQLXML
  def sqlxml(index: Int): SQLXML
  def string(column: String): String
  def string(index: Int): String
  def time(column: String): Time
  def time(column: String, cal: Calendar): Time
  def time(index: Int): Time
  def time(index: Int, cal: Calendar): Time
  def timestamp(column: String): Timestamp
  def timestamp(column: String, cal: Calendar): Timestamp
  def timestamp(index: Int): Timestamp
  def timestamp(index: Int, cal: Calendar): Timestamp
  def rstype: ResultSetType
  def url(column: String): URL
  def url(index: Int): URL
  def warnings: SQLWarning
  def isAfterLast: Boolean
  def isBeforeFirst: Boolean
  def isClosed: Boolean
  def isFirst: Boolean
  def isLast: Boolean
  def rowDeleted: Boolean
  def rowInserted: Boolean
  def rowUpdated: Boolean
  def wasNull: Boolean
}

/**
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ImmutableResultSet {
  implicit def resultset(r: ResultSet) = new ImmutableResultSet {
    import java.util.Calendar
    def findColumn(column: String) = r findColumn column
    def array(column: String) = r getArray column
    def array(index: Int) = r getArray index
    def asciiStream(column: String) = r getAsciiStream column
    def asciiStream(index: Int) = r getAsciiStream index
    def bigDecimal(column: String) = r getBigDecimal column
    def bigDecimal(index: Int) = r getBigDecimal index
    def binaryStream(column: String) = r getBinaryStream column
    def binaryStream(index: Int) = r getBinaryStream index
    def blob(column: String) = r getBlob column
    def blob(index: Int) = r getBlob index
    def boolean(column: String) = r getBoolean column
    def boolean(index: Int) = r getBoolean index
    def byte(column: String) = r getByte column
    def byte(index: Int) = r getByte index
    def bytes(column: String) = r getBytes column
    def bytes(index: Int) = r getBytes index
    def characterStream(column: String) = r getCharacterStream column
    def characterStream(index: Int) = r getCharacterStream index
    def clob(column: String) = r getClob column
    def clob(index: Int) = r getClob index
    def concurrency = ResultSetConcurrencyType.fromInt(r getConcurrency).get
    def cursor = r getCursorName
    def date(column: String) = r getDate column
    def date(column: String, cal: Calendar) = r getDate (column, cal)
    def date(index: Int) = r getDate index
    def date(index: Int, cal: Calendar) = r getDate (index, cal)
    def double(column: String) = r getDouble column
    def double(index: Int) = r getDouble index
    def fetchDirection = ResultSetFetchDirection.fromInt(r getFetchDirection).get
    def fetchSize = r getFetchSize
    def float(column: String) = r getFloat column
    def float(index: Int) = r getFloat index
    def holdability = ResultSetHoldabilityType.fromInt(r getHoldability).get
    def int(column: String) = r getInt column
    def int(index: Int) = r getInt index
    def long(column: String) = r getLong column
    def long(index: Int) = r getLong index
    def metadata = r getMetaData
    def ncharacterStream(column: String) = r getNCharacterStream column
    def ncharacterStream(index: Int) = r getNCharacterStream index
    def nclob(column: String) = r getNClob column
    def nclob(index: Int) = r getNClob index
    def nstring(column: String) = r getNString column
    def nstring(index: Int) = r getNString index
    def value(column: String) = r getObject column
    def value(column: String, m: Map[String, Class[_]]) = r getObject (column, m)
    def value(index: Int) = r getObject index
    def value(index: Int, m: Map[String, Class[_]]) = r getObject (index, m)
    def ref(column: String) = r getRef column
    def ref(index: Int) = r getRef index
    def row = r getRow
    def rowId(column: String) = r getRowId column
    def rowId(index: Int) = r getRowId index
    def short(column: String) = r getShort column
    def short(index: Int) = r getShort index
    def sqlxml(column: String) = r getSQLXML column
    def sqlxml(index: Int) = r getSQLXML index
    def string(column: String) = r getString column
    def string(index: Int) = r getString index
    def time(column: String) = r getTime column
    def time(column: String, cal: Calendar) = r getTime column
    def time(index: Int) = r getTime index
    def time(index: Int, cal: Calendar) = r getTime (index, cal)
    def timestamp(column: String) = r getTimestamp column
    def timestamp(column: String, cal: Calendar) = r getTimestamp (column, cal)
    def timestamp(index: Int) = r getTimestamp index
    def timestamp(index: Int, cal: Calendar) = r getTimestamp (index, cal)
    def rstype = ResultSetType.fromInt(r getType).get
    def url(column: String) = r getURL column
    def url(index: Int) = r getURL index
    def warnings = r getWarnings
    def isAfterLast = r isAfterLast
    def isBeforeFirst = r isBeforeFirst
    def isClosed = r isClosed
    def isFirst = r isFirst
    def isLast = r isLast
    def rowDeleted = r rowDeleted
    def rowInserted = r rowInserted
    def rowUpdated = r rowUpdated
    def wasNull = r wasNull
  }
}
