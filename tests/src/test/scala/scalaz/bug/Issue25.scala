package scalaz.bug

import org.specs.{Specification, Sugar, ScalaCheck}
import scalaz._
import Scalaz._
import org.specs.specification.PendingUntilFixed

/** https://github.com/scalaz/scalaz/issues/25 */
object Issue25 extends Specification with Sugar with ScalaCheck with PendingUntilFixed {

  val f = 1 + (_: Int)

  "function1 semigroup" in {
    val f2 = f |+| f
    f2(7) must be_==(16)
  }

  "function1 semigroup 1" in {
    val f2 = mzero[(Int => Int)] |+| f |+| f
    f2(7) must be_==(16)
  }

  "function1 semigroup 2" in {
    val f2 = List(f, f).suml
    f2(7) must be_==(16)
  }

  "function1 semigroup 3" in {
    val f2 = Seq(f, f).sumr
    f2(7) must be_==(16)
  }

  "function1 semigroup 4" in {
    val f2 = List(f, f).sumr
    f2(7) must be_==(16)
  }
}