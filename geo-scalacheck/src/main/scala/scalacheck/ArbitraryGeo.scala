package scalaz
package scalacheck

import Scalaz._
import org.scalacheck.{Pretty, Gen, Arbitrary}

import Arbitrary._
import Gen._

object ArbitraryGeo {
  import geo._
  import Geo._
  import ScalaCheckBinding._

  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  val arbDouble = implicitly[Arbitrary[Double]]

  implicit def AzimuthArbitrary: Arbitrary[Azimuth] = arbDouble ∘ (azimuth _)

  implicit def BearingArbitrary: Arbitrary[Bearing] = arbDouble ∘ (bearing _)

  implicit def CoordArbitrary: Arbitrary[Coord] = arb[Latitude].<**>(arb[Longitude])(coord _)

  implicit def ElevatedCurveArbitrary: Arbitrary[ElevatedCurve] = arb[GeodeticCurve].<**>(arb[Elevation])(elevatedCurve _)

  implicit def ElevationArbitrary: Arbitrary[Elevation] = arbDouble ∘ (elevation _)

  implicit def EllipsoidArbitrary: Arbitrary[Ellipsoid] = arbDouble.<****>(arbDouble, arbDouble, arbDouble)(ellipsoid _)

  implicit def GeodeticCurveArbitrary: Arbitrary[GeodeticCurve] = arbDouble.<***>(arb[Azimuth], arb[Azimuth])(curve _)

  implicit def LatitudeArbitrary: Arbitrary[Latitude] = arbDouble ∘ (latitude _)

  implicit def LongitudeArbitrary: Arbitrary[Longitude] = arbDouble ∘ (longitude _)

  implicit def PositionArbitrary: Arbitrary[Position] = arb[Coord].<**>(arb[Elevation])(position _)

  implicit def VectorArbitrary: Arbitrary[Vector] = arb[Coord].<**>(arb[Bearing])(vector _)

}
