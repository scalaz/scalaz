package scalaz

import std.AllInstances._
import syntax.apply._
import std.option.some
import scalaz.scalacheck.ScalazProperties.applicative
import scalaz.scalacheck.ScalazArbitrary._

object ApplyTest extends SpecLite {
  checkAll("List applyApplicative", {
             implicit val F = Apply[List].applyApplicative
             applicative.laws[λ[α => List[α] \/ α]]
           })

  "mapN" in {
    Apply[Option].apply2(some("1"), some("2"))(_ + _) must_===(some("12"))
    Apply[Option].apply3(some("1"), some("2"), some("3"))(_ + _ + _) must_===(some("123"))
    Apply[Option].apply4(some("1"), some("2"), some("3"), some("4"))(_ + _ + _ + _) must_===(some("1234"))
    Apply[Option].apply5(some("1"), some("2"), some("3"), some("4"), some("5"))(_ + _ + _ + _ + _) must_===(some("12345"))

    val A = Monoid[String].applicative
    def undefined = sys.error("")

    A.ap(fa = "1")(f = "2") must_===("21")
    A.ap2("1", "2")("3") must_===("312")
    A.ap3("1", "2", "3")("4") must_===("4123")
    A.ap4("1", "2", "3", "4")("5") must_===("51234")
    A.ap5("1", "2", "3", "4", "5")("6") must_===("612345")

    A.apply2("1", "2")((a, b) => undefined) must_===("12")
    A.apply3("1", "2", "3")((a, b, c) => undefined) must_===("123")
    A.apply4("1", "2", "3", "4")((a, b, c, d) => undefined) must_===("1234")
    A.apply5("1", "2", "3", "4", "5")((a, b, c, d, e) => undefined) must_===("12345")
  }

  "apN" in {
    Apply[Option].ap2(some("1"), some("2"))(some((_: String) + (_: String))) must_===(some("12"))
    Apply[Option].ap3(some("1"), some("2"), some("3"))(some((_: String) + (_: String) + (_: String))) must_===(some("123"))
    Apply[Option].ap4(some("1"), some("2"), some("3"), some("4"))(some((_: String) + (_: String) + (_: String) + (_: String))) must_===(some("1234"))
    Apply[Option].ap5(some("1"), some("2"), some("3"), some("4"), some("5"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("12345"))
  }

  "<*>" in {
    some(9) <*> some({(_: Int) + 3}) must_===(some(12))
  }
}
