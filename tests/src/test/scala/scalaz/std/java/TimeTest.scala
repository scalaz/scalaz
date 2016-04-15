package scalaz.std.java

import scalaz.{SpecLite, Apply}
import java.time._
import org.scalacheck._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.java.time._

object TimeTest extends SpecLite {

  private[this] def gen[A](implicit A: Arbitrary[A]): Gen[A] = A.arbitrary

  private[this] val smallIntArb = Arbitrary(Gen.choose(1, 100000))

  implicit val DurationArbitrary: Arbitrary[Duration] =
    Arbitrary(Gen.oneOf(
      gen[Long].map{Duration.ofNanos},
      gen[Long].map{Duration.ofMillis},
      gen[Int].map{Duration.ofSeconds(_)}
    ))

  implicit val PeriodArbitrary: Arbitrary[Period] =
    Apply[Arbitrary].apply3(smallIntArb, smallIntArb, smallIntArb)(Period.of(_, _, _))

  implicit val LocalDateArbitrary: Arbitrary[LocalDate] =
    Arbitrary(Apply[Gen].apply3(
      Gen.choose(Year.MIN_VALUE, Year.MAX_VALUE), Gen.choose(1, 12), Gen.choose(1, 28)
    )(LocalDate.of(_, _, _)))

  implicit val LocalTimeArbitrary: Arbitrary[LocalTime] =
    Arbitrary(Apply[Gen].apply4(
      Gen.choose(0, 23), Gen.choose(0, 59), Gen.choose(0, 59), Gen.choose(0, 999999999)
    )(LocalTime.of(_, _, _, _)))

  implicit val YearArbitrary: Arbitrary[Year] =
    Arbitrary(Gen.choose(Year.MIN_VALUE, Year.MAX_VALUE).map(Year.of(_)))

  implicit val YearMonthArbitrary: Arbitrary[YearMonth] =
    Arbitrary(
      Apply[Gen].apply2(Gen.choose(Year.MIN_VALUE, Year.MAX_VALUE), Gen.choose(1, 12))(YearMonth.of(_, _))
    )

  implicit val MonthDayArbitrary: Arbitrary[MonthDay] =
    Arbitrary(
      Apply[Gen].apply2(Gen.choose(1, 12), Gen.choose(1, 28))(MonthDay.of(_, _))
    )

  implicit val monthArbitrary: Arbitrary[Month] =
    Arbitrary(Gen.oneOf(Month.values))

  checkAll("Duration", monoid.laws[Duration])
  checkAll("Duration", order.laws[Duration])

  checkAll("Period", monoid.laws[Period])
  checkAll("Period", equal.laws[Period])
  checkAll("YearMonth", ScalazProperties.enum.laws[YearMonth])
  checkAll("MonthDay", order.laws[MonthDay])
  checkAll("Month", ScalazProperties.enum.laws[Month])
  checkAll("LocalTime", order.laws[LocalTime])

  checkAll("LocalDate", ScalazProperties.enum.laws[LocalDate])
  checkAll("Year", ScalazProperties.enum.laws[Year])
}
