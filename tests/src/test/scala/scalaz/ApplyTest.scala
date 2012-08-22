package scalaz

import std.AllInstances._
import syntax.apply._
import std.option.some
import syntax.traverse._

class ApplyTest extends Spec {
  "mapN" in {
    Apply[Option].apply(some("1"), some("2"))(_ + _) must be_===(some("12"))
    Apply[Option].apply(some("1"), some("2"), some("3"))(_ + _ + _) must be_===(some("123"))
    Apply[Option].apply(some("1"), some("2"), some("3"), some("4"))(_ + _ + _ + _) must be_===(some("1234"))
    Apply[Option].apply(some("1"), some("2"), some("3"), some("4"), some("5"))(_ + _ + _ + _ + _) must be_===(some("12345"))
    Apply[Option].apply(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("10"), some("11"), some("12"), some("13"), some("14"), some("15"), some("16"), some("17"), some("18"), some("19"), some("20"), some("21"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must be_===(some("123456789101112131415161718192021"))

    val A = Monoid[String].applicative
    def undefined = sys.error("")

    A.ap(fa = "1")(f = "2") must be_===("21")
    A.ap("1", "2")("3") must be_===("312")
    A.ap("1", "2", "3")("4") must be_===("4123")
    A.ap("1", "2", "3", "4")("5") must be_===("51234")
    A.ap("1", "2", "3", "4", "5")("6") must be_===("612345")

    A("1", "2")((a, b) => undefined) must be_===("12")
    A("1", "2", "3")((a, b, c) => undefined) must be_===("123")
    A("1", "2", "3", "4")((a, b, c, d) => undefined) must be_===("1234")
    A("1", "2", "3", "4", "5")((a, b, c, d, e) => undefined) must be_===("12345")
  }

  "apN" in {
    Apply[Option].ap(some("1"), some("2"))(some((_: String) + (_: String))) must be_===(some("12"))
    Apply[Option].ap(some("1"), some("2"), some("3"))(some((_: String) + (_: String) + (_: String))) must be_===(some("123"))
    Apply[Option].ap(some("1"), some("2"), some("3"), some("4"))(some((_: String) + (_: String) + (_: String) + (_: String))) must be_===(some("1234"))
    Apply[Option].ap(some("1"), some("2"), some("3"), some("4"), some("5"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must be_===(some("12345"))
  }
}
