package scalaz
package std.java

import java.time._

trait TimeInstances0 {

  private[this] def orderFromInt[A](f: (A, A) => Int): Order[A] = new Order[A] {
    def order(x: A, y: A) = Ordering.fromInt(f(x, y))
  }

  implicit val instantInstance: Order[Instant] = orderFromInt[Instant](_ compareTo _)
  implicit val localDateTimeInstance: Order[LocalDateTime] = orderFromInt[LocalDateTime](_ compareTo _)
  implicit val offsetDateTimeInstance: Order[OffsetDateTime] = orderFromInt[OffsetDateTime](_ compareTo _)
  implicit val offsetTimeInstance: Order[OffsetTime] = orderFromInt[OffsetTime](_ compareTo _)
  implicit val zonedDateTime: Order[ZonedDateTime] = orderFromInt[ZonedDateTime](_ compareTo _)
  implicit val zoneOffsetInstance: Order[ZoneOffset] = orderFromInt[ZoneOffset](_ compareTo _)

  implicit val dayOfWeekInstance: Enum[DayOfWeek] = new Enum[DayOfWeek] {
    override val max = Some(DayOfWeek.SUNDAY)
    override val min = Some(DayOfWeek.MONDAY)
    override def pred(a: DayOfWeek) = a.minus(1)
    override def succ(a: DayOfWeek) = a.plus(1)
    override def order(x: DayOfWeek, y: DayOfWeek) =
      Ordering.fromInt(x compareTo y)
  }
}
