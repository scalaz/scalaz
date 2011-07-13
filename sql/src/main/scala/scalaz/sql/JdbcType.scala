package scalaz
package sql

import java.util.Calendar
import java.io.{Reader, InputStream}
import java.net.URL
import java.sql.{Clob, Blob, Ref, Timestamp, Time, Date}

/**
 * Denotes all the different types that can be set on a SQL statement (compound disjoint union).
 *
 * @see java.sql.PreparedStatement
 * @author Tony Morris <tmorris@tmorris.net>
 */
sealed trait JdbcType {
  /**
   * Reduction (catamorphism) on the different JDBC types.
   *
   * @tparam X The return type used in picking one (and only one) of the given reduction functions.
   * @param nullType If the type is `null` with a specific sql-type.
   * @param booleanType If the type is `Boolean`.
   * @param byteType If the type is `Byte`.
   * @param shortType If the type is `Short`.
   * @param intType If the type is `Int`.
   * @param longType If the type is `Long`.
   * @param floatType If the type is `Float`.
   * @param doubleType If the type is `Double`.
   * @param bigDecimalType If the type is `BigDecimal`.
   * @param stringType If the type is `String`.
   * @param bytesType If the type is `Array[Byte]`.
   * @param dateType If the type is `Date`.
   * @param timeType If the type is `Time`.
   * @param timestampType If the type is `Timestamp`.
   * @param asciiStreamType If the type is an ASCII `InputStream` taking a specific number of bytes.
   * @param binaryStreamType If the type is a binary `InputStream` taking a specific number of bytes.
   * @param objectTypeType If the type is an 'Object` with a specific sql target type.
   * @param objectType If the type is 'Object`.
   * @param characterStreamType If the type is a `Reader` taking a specific number of characters.
   * @param refType If the type is `Ref`.
   * @param blobType If the type is `Blob`.
   * @param clobType If the type is `Clob`.
   * @param arrayType If the type is sql `Array`.
   * @param calendarDateType If the type is `Date` with a specific `Calendar`.
   * @param calendarTimeType If the type is `Time` with a specific `Calendar`.
   * @param calendarTimestampType If the type is `Timestamp` with a specific `Calendar`.
   * @param userNullType If the type is `null` with a fully-qualified name of an SQL user-defined type.
   * @param urlType If the type is `URL`.
   * @return The result of reduction on one and only one of the given function values.
   */
  def fold[X](
               nullType: SqlType => X
             , booleanType: Boolean => X
             , byteType: Byte => X
             , shortType: Short => X
             , intType: Int => X
             , longType: Long => X
             , floatType: Float => X
             , doubleType: Double => X
             , bigDecimalType: java.math.BigDecimal => X
             , stringType: String => X
             , bytesType: Array[Byte] => X
             , dateType: Date => X
             , timeType: Time => X
             , timestampType: Timestamp => X
             , asciiStreamType: InputStream => Int => X
             , binaryStreamType: InputStream => Int => X
             , objectTypeType: AnyRef => SqlType => X
             , objectType: AnyRef => X
             , characterStreamType: Reader => Int => X
             , refType: Ref => X
             , blobType: Blob => X
             , clobType: Clob => X
             , arrayType: java.sql.Array => X
             , calendarDateType: Date => Calendar => X
             , calendarTimeType: Time => Calendar => X
             , calendarTimestampType: Timestamp => Calendar => X
             , userNullType: SqlType => String => X
             , urlType: URL => X
             ) =
  this match {
    case NullJdbcType(typ) => nullType(typ)
    case BooleanJdbcType(value) => booleanType(value)
    case ByteJdbcType(value) => byteType(value)
    case ShortJdbcType(value) => shortType(value)
    case IntJdbcType(value) => intType(value)
    case LongJdbcType(value) => longType(value)
    case FloatJdbcType(value) => floatType(value)
    case DoubleJdbcType(value) => doubleType(value)
    case BigDecimalJdbcType(value) => bigDecimalType(value)
    case StringJdbcType(value) => stringType(value)
    case BytesJdbcType(value) => bytesType(value)
    case DateJdbcType(value) => dateType(value)
    case TimeJdbcType(value) => timeType(value)
    case TimestampJdbcType(value) => timestampType(value)
    case AsciiStreamJdbcType(value, length) => asciiStreamType(value)(length)
    case BinaryStreamJdbcType(value, length) => binaryStreamType(value)(length)
    case ObjectTypeJdbcType(value, typ) => objectTypeType(value)(typ)
    case ObjectJdbcType(value) => objectType(value)
    case CharacterStreamJdbcType(value, length) => characterStreamType(value)(length)
    case RefJdbcType(value) => refType(value)
    case BlobJdbcType(value) => blobType(value)
    case ClobJdbcType(value) => clobType(value)
    case ArrayJdbcType(value) => arrayType(value)
    case CalendarDateJdbcType(value, cal) => calendarDateType(value)(cal)
    case CalendarTimeJdbcType(value, cal) => calendarTimeType(value)(cal)
    case CalendarTimestampJdbcType(value, cal) => calendarTimestampType(value)(cal)
    case UserNullJdbcType(typ, name) => userNullType(typ)(name)
    case URLJDBCType(value) => urlType(value)
  }
}
private case class NullJdbcType(typ: SqlType) extends JdbcType
private case class BooleanJdbcType(value: Boolean) extends JdbcType
private case class ByteJdbcType(value: Byte) extends JdbcType
private case class ShortJdbcType(value: Short) extends JdbcType
private case class IntJdbcType(value: Int) extends JdbcType
private case class LongJdbcType(value: Long) extends JdbcType
private case class FloatJdbcType(value: Float) extends JdbcType
private case class DoubleJdbcType(value: Double) extends JdbcType
private case class BigDecimalJdbcType(value: java.math.BigDecimal) extends JdbcType
private case class StringJdbcType(value: String) extends JdbcType
private case class BytesJdbcType(value: Array[Byte]) extends JdbcType
private case class DateJdbcType(value: Date) extends JdbcType
private case class TimeJdbcType(value: Time) extends JdbcType
private case class TimestampJdbcType(value: Timestamp) extends JdbcType
private case class AsciiStreamJdbcType(value: InputStream, length: Int) extends JdbcType
private case class BinaryStreamJdbcType(value: InputStream, length: Int) extends JdbcType
private case class ObjectTypeJdbcType(value: AnyRef, typ: SqlType) extends JdbcType
private case class ObjectJdbcType(value: AnyRef) extends JdbcType
private case class CharacterStreamJdbcType(value: Reader, length: Int) extends JdbcType
private case class RefJdbcType(value: Ref) extends JdbcType
private case class BlobJdbcType(value: Blob) extends JdbcType
private case class ClobJdbcType(value: Clob) extends JdbcType
private case class ArrayJdbcType(value: java.sql.Array) extends JdbcType
private case class CalendarDateJdbcType(value: Date, cal: Calendar) extends JdbcType
private case class CalendarTimeJdbcType(value: Time, cal: Calendar) extends JdbcType
private case class CalendarTimestampJdbcType(value: Timestamp, cal: Calendar) extends JdbcType
private case class UserNullJdbcType(typ: SqlType, name: String) extends JdbcType
private case class URLJDBCType(value: URL) extends JdbcType

object JdbcType extends JDBCTypes

trait JDBCTypes {
  /**
   * Construct a `null` type.
   */
  def nullType: SqlType => JdbcType = NullJdbcType(_)

  /**
   * Construct a `Boolean` type.
   */
  def booleanType: Boolean => JdbcType = BooleanJdbcType(_)

  /**
   * Construct a `Byte` type.
   */
  def byteType: Byte => JdbcType = ByteJdbcType(_)

  /**
   * Construct a `Short` type.
   */
  def shortType: Short => JdbcType = ShortJdbcType(_)

  /**
   * Construct a `Int` type.
   */
  def intType: Int => JdbcType = IntJdbcType(_)

  /**
   * Construct a `Long` type.
   */
  def longType: Long => JdbcType = LongJdbcType(_)

  /**
   * Construct a `Float` type.
   */
  def floatType: Float => JdbcType = FloatJdbcType(_)

  /**
   * Construct a `Double` type.
   */
  def doubleType: Double => JdbcType = DoubleJdbcType(_)

  /**
   * Construct a `BigDecimal` type.
   */
  def bigDecimalType: java.math.BigDecimal => JdbcType = BigDecimalJdbcType(_)

  /**
   * Construct a `String` type.
   */
  def stringType: String => JdbcType = StringJdbcType(_)

  /**
   * Construct an `Array[Byte]` type.
   */
  def bytesType: Array[Byte] => JdbcType = BytesJdbcType(_)

  /**
   * Construct a `Date` type.
   */
  def dateType: Date => JdbcType = DateJdbcType(_)

  /**
   * Construct a `Time` type.
   */
  def timeType: Time => JdbcType = TimeJdbcType(_)

  /**
   * Construct a `Timestamp` type.
   */
  def timestampType: Timestamp => JdbcType = TimestampJdbcType(_)

  /**
   * Construct an ASCII `InputStream` type taking the given number of bytes.
   */
  def asciiStreamType: (InputStream, Int) => JdbcType = AsciiStreamJdbcType(_, _)

  /**
   * Construct a binary `InputStream` type taking the given number of bytes.
   */
  def binaryStreamType: (InputStream, Int) => JdbcType = BinaryStreamJdbcType(_, _)

  /**
   * Construct an 'Object` with a specific sql target type.
   */
  def objectTypeType: (AnyRef, SqlType) => JdbcType = ObjectTypeJdbcType(_, _)

  /**
   * Construct an `Object` type.
   */
  def objectType: AnyRef => JdbcType = ObjectJdbcType(_)

  /**
   * Construct a `Reader` type taking the given number of characters.
   */
  def characterStreamType: (Reader, Int) => JdbcType = CharacterStreamJdbcType(_, _)

  /**
   * Construct a `Ref` type.
   */
  def refType: Ref => JdbcType = RefJdbcType(_)

  /**
   * Construct a `Blob` type.
   */
  def blobType: Blob => JdbcType = BlobJdbcType(_)

  /**
   * Construct a `Clob` type.
   */
  def clobType: Clob => JdbcType = ClobJdbcType(_)

  /**
   * Construct an sql `Array` type.
   */
  def arrayType: java.sql.Array => JdbcType = ArrayJdbcType(_)

  /**
   * Construct a `Date` type with a specific `Calendar`.
   */
  def calendarDateType: (Date, Calendar) => JdbcType = CalendarDateJdbcType(_, _)

  /**
   * Construct a `Time` type with a specific `Calendar`.
   */
  def calendarTimeType: (Time, Calendar) => JdbcType = CalendarTimeJdbcType(_, _)

  /**
   * Construct a `Timestamp` type with a specific `Calendar`.
   */
  def calendarTimestampType: (Timestamp, Calendar) => JdbcType = CalendarTimestampJdbcType(_, _)

  /**
   * Construct a `null` type with a fully-qualified name of an SQL user-defined type.
   */
  def userNullType: (SqlType, String) => JdbcType = UserNullJdbcType(_, _)

  /**
   * Construct a `URL` type.
   */
  def urlType: URL => JdbcType = URLJDBCType(_)

  implicit val JDBCTypeShow: Show[JdbcType] = Show.showA
}
