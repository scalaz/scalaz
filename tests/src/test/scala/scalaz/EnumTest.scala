package scalaz

import scalaz.std.AllInstances._
import scalaz.syntax.`enum`._

object EnumTest extends SpecLite {

  "|--> with step 1" in {
    val expected = IList(120.toByte, 121.toByte, 122.toByte, 123.toByte, 124.toByte, 125.toByte, 126.toByte, 127.toByte)
    val generated = 120.toByte |--> (1, 127.toByte)
    assert(generated === expected)
  }

  "|--> with step 2" in {
    assert((120.toByte |--> (2, 126.toByte)) === IList(120.toByte, 122.toByte, 124.toByte, 126.toByte))
  }

  "|--> with step -2" in {
    val expected = IList(-120.toByte, -122.toByte, -124.toByte, -126.toByte)
    val generated = -120.toByte |--> (-2, -126.toByte)
    assert(generated === expected)
  }

  "|--> with step 4" in {
    assert((120.toByte |--> (4, 127.toByte)) === IList(120.toByte, 124.toByte))
  }

  "|--> with step -8" in {
    assert((-120.toByte |--> (-8, -128.toByte)) === IList(-120.toByte, -128.toByte))
  }
}
