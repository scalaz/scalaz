package scalaz

import scalaz.Scalaz._
import scalaz.scalacheck.ScalazProperties._

object ProfunctorSpec extends SpecLite {

  implicit def EqualFunction1 = Equal.equalBy[Int => Int, Int](_.apply(0))

  checkAll("Profunctor Function1", profunctor.laws[? => ?])
}
