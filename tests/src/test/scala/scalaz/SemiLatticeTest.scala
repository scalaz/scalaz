package scalaz

import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazProperties.semilattice
import Tags._
import BandTest._

object SemiLatticeTest extends SpecLite {

  checkAll(semilattice.laws[Int @@ MinVal])
  checkAll(semilattice.laws[Int @@ MaxVal])

}
