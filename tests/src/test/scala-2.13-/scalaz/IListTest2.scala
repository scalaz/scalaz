package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object IListTest2 extends SpecLite {
  "lastIndexOfSlice" ! forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.lastIndexOfSlice(ms).getOrElse(-1) must_=== ns.toList.lastIndexOfSlice(ms.toList)
  }
}
