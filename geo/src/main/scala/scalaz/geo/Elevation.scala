package scalaz
package geo

sealed trait Elevation {
  val value: Double
}

trait Elevations {
  def elevation(d: Double) = new Elevation {
    val value = d.abs
  }
}

object Elevation {
  import Predef.{implicitly => i}
  import Scalaz._

  implicit def ElevationShow: Show[Elevation] = shows(_.value.shows + "m")

  implicit def ElevationEqual: Equal[Elevation] = i[Equal[Double]] ∙ (_.value)

  implicit def ElevationOrder: Order[Elevation] = i[Order[Double]] ∙ (_.value)
}