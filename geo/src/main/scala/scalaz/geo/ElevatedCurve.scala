package scalaz
package geo

sealed trait ElevatedCurve {
  val curve: GeodeticCurve
  val elevation: Elevation

  lazy val length = {
    val d = curve.ellipsoidalDistance
    val e = elevation.value
    scala.math.sqrt(d * d + e * e) 
  }
}

trait ElevatedCurves {
  def elevatedCurve(c: GeodeticCurve, e: Elevation) = new ElevatedCurve {
    val curve = c
    val elevation = e
  }
}

object ElevatedCurve {
  import Predef.{implicitly => i}
  import Scalaz._
  
  implicit def ElevatedCurveShow: Show[ElevatedCurve] = i[Show[(GeodeticCurve, Elevation)]] ∙ (((_: ElevatedCurve).curve) &&& ((_: ElevatedCurve).elevation))

  implicit def ElevatedCurveEqual: Equal[ElevatedCurve] = i[Equal[(GeodeticCurve, Elevation)]] ∙ (((_: ElevatedCurve).curve) &&& ((_: ElevatedCurve).elevation))

  implicit def ElevatedCurveOrder: Order[ElevatedCurve] = i[Order[(GeodeticCurve, Elevation)]] ∙ (((_: ElevatedCurve).curve) &&& ((_: ElevatedCurve).elevation))
}