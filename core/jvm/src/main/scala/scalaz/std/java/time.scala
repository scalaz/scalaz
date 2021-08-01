package scalaz
package std.java

import java.time._

trait TimeInstances {

  private[this] def orderFromInt[A](f: (A, A) => Int): Order[A] =
    (x: A, y: A) => Ordering.fromInt(f(x, y))

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

  implicit val instantInstance: Order[Instant] = orderFromInt[Instant](_ compareTo _)

  implicit val durationInstance: Monoid[Duration] with Order[Duration] =
    new Monoid[Duration] with Order[Duration] {
      override def zero = Duration.ZERO
      override def append(f1: Duration, f2: => Duration) = f1 plus f2
      override def order(a1: Duration, a2: Duration) = Ordering.fromInt(a1 compareTo a2)
    }

  implicit val periodInstance: Monoid[Period] with Equal[Period] =
    new Monoid[Period] with Equal[Period] {
      override def zero = Period.ZERO
      override def append(f1: Period, f2: => Period) = f1 plus f2
      override def equal(a1: Period, a2: Period) = a1 == a2
    }

  implicit val yearMonthInstance: Enum[YearMonth] = new Enum[YearMonth] {
    override def pred(a: YearMonth) = a.minusMonths(1)
    override def succ(a: YearMonth) = a.plusMonths(1)
    override def order(x: YearMonth, y: YearMonth) =
      Ordering.fromInt(x compareTo y)
  }

  implicit val monthDayInstance: Order[MonthDay] = orderFromInt[MonthDay](_ compareTo _)
  implicit val localTimeInstance: Order[LocalTime] = orderFromInt[LocalTime](_ compareTo _)

  implicit val yearInstance: Enum[Year] = new Enum[Year] {
    override def pred(a: Year) = a.minusYears(1)
    override def succ(a: Year) = a.plusYears(1)
    override def order(x: Year, y: Year) =
      Ordering.fromInt(x compareTo y)
  }

  implicit val localDateInstance: Enum[LocalDate] = new Enum[LocalDate] {
    override def pred(a: LocalDate) = a.minusDays(1)
    override def succ(a: LocalDate) = a.plusDays(1)
    override def order(x: LocalDate, y: LocalDate) =
      Ordering.fromInt(x compareTo y)
  }

  implicit val monthInstance: Enum[Month] = new Enum[Month] {
    override val max = Some(Month.DECEMBER)
    override val min = Some(Month.JANUARY)
    override def pred(a: Month): Month = a.minus(1)
    override def succ(a: Month): Month = a.plus(1)
    override def order(x: Month, y: Month) =
      Ordering.fromInt(x compareTo y)
  }

}

object time extends TimeInstances
