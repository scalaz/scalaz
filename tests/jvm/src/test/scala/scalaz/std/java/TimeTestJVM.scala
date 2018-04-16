package scalaz.std.java

import scalaz.{SpecLite, Apply}
import java.time._
import org.scalacheck._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazProperties
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.java.time._
import scala.collection.JavaConverters._
import TimeTest._

object TimeTestJVM extends SpecLite {

  private[this] def arb[A](implicit A: Arbitrary[A]): Arbitrary[A] = A

  implicit val LocalDateTimeArbitrary: Arbitrary[LocalDateTime] =
    Arbitrary(Apply[Gen].apply7(
      Gen.choose(Year.MIN_VALUE, Year.MAX_VALUE),
      Gen.choose(1, 12),
      Gen.choose(1, 28),
      Gen.choose(0, 23),
      Gen.choose(0, 59),
      Gen.choose(0, 59),
      Gen.choose(0, 999999999)
    )(LocalDateTime.of(_, _, _, _, _, _, _)))

  implicit val zonedOffsetArbitrary: Arbitrary[ZoneOffset] =
    Arbitrary(
      Apply[Gen].apply3(Gen.choose(0, 17), Gen.choose(0, 59), Gen.choose(0, 59))(
        ZoneOffset.ofHoursMinutesSeconds
      )
    )

  implicit val zoneIdArbitrary: Arbitrary[ZoneId] =
    Arbitrary(
      Gen.oneOf(ZoneId.getAvailableZoneIds.asScala.map(ZoneId.of).toList)
    )

  implicit val offsetDateTimeArbitrary: Arbitrary[OffsetDateTime] =
    Apply[Arbitrary].apply2(arb[LocalDateTime], arb[ZoneOffset])(
      OffsetDateTime.of(_, _)
    )

  implicit val offsetTimeArbitrary: Arbitrary[OffsetTime] =
    Apply[Arbitrary].apply2(arb[LocalTime], arb[ZoneOffset])(
      OffsetTime.of(_, _)
    )

  implicit val zonedDateTimeArbitrary: Arbitrary[ZonedDateTime] =
    Apply[Arbitrary].apply2(arb[LocalDateTime], arb[ZoneId])(
      ZonedDateTime.of(_, _)
    )

  checkAll("DayOfWeek", ScalazProperties.enum.laws[DayOfWeek])
  checkAll("LocalDateTime", order.laws[LocalDateTime])
  checkAll("OffsetDateTime", order.laws[OffsetDateTime])
  checkAll("OffsetTime", order.laws[OffsetTime])
  checkAll("ZonedDateTime", order.laws[ZonedDateTime])
  checkAll("ZoneOffset", order.laws[ZoneOffset])
}
