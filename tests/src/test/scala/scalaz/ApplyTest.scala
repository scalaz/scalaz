package scalaz

import std.AllInstances._
import syntax.apply._
import std.option.some
import scalaz.scalacheck.ScalazProperties.applicative
import scalaz.scalacheck.ScalazArbitrary._

object ApplyTest extends SpecLite {
  checkAll("List applyApplicative", {
             implicit val F: Applicative[λ[α => List[α] \/ α]] = Apply[List].applyApplicative
             applicative.laws[λ[α => List[α] \/ α]]
           })

  "mapN" in {
    Apply[Option].apply2(some("1"), some("2"))(_ + _) must_===(some("12"))
    Apply[Option].apply3(some("1"), some("2"), some("3"))(_ + _ + _) must_===(some("123"))
    Apply[Option].apply4(some("1"), some("2"), some("3"), some("4"))(_ + _ + _ + _) must_===(some("1234"))
    Apply[Option].apply5(some("1"), some("2"), some("3"), some("4"), some("5"))(_ + _ + _ + _ + _) must_===(some("12345"))
    Apply[Option].apply6(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"))(_ + _ + _ + _ + _ + _) must_===(some("123456"))
    Apply[Option].apply7(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"))(_ + _ + _ + _ + _ + _ + _) must_===(some("1234567"))
    Apply[Option].apply8(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"))(_ + _ + _ + _ + _ + _ + _ + _) must_===(some("12345678"))
    Apply[Option].apply9(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"))(_ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789"))
    Apply[Option].apply10(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789A"))
    Apply[Option].apply11(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789AB"))
    Apply[Option].apply12(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABC"))
    Apply[Option].apply13(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABCD"))
    Apply[Option].apply14(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABCDE"))
    Apply[Option].apply15(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABCDEF"))
    Apply[Option].apply16(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABCDEFG"))
    Apply[Option].apply17(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABCDEFGH"))
    Apply[Option].apply18(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABCDEFGHI"))
    Apply[Option].apply19(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"), some("J"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABCDEFGHIJ"))
    Apply[Option].apply20(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"), some("J"), some("K"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABCDEFGHIJK"))
    Apply[Option].apply21(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"), some("J"), some("K"), some("L"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABCDEFGHIJKL"))
    Apply[Option].apply22(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"), some("J"), some("K"), some("L"), some("M"))(_ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _ + _) must_===(some("123456789ABCDEFGHIJKLM"))

    val A = Monoid[String].applicative
    def undefined = sys.error("")

    A.ap(fa = "1")(f = "2") must_===("21")
    A.ap2("1", "2")("3") must_===("312")
    A.ap3("1", "2", "3")("4") must_===("4123")
    A.ap4("1", "2", "3", "4")("5") must_===("51234")
    A.ap5("1", "2", "3", "4", "5")("6") must_===("612345")
    A.ap6("1", "2", "3", "4", "5", "6")("7") must_===("7123456")
    A.ap7("1", "2", "3", "4", "5", "6", "7")("8") must_===("81234567")
    A.ap8("1", "2", "3", "4", "5", "6", "7", "8")("9") must_===("912345678")
    A.ap9("1", "2", "3", "4", "5", "6", "7", "8", "9")("A") must_===("A123456789")
    A.ap10("1", "2", "3", "4", "5", "6", "7", "8", "9", "A")("B") must_===("B123456789A")
    A.ap11("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B")("C") must_===("C123456789AB")
    A.ap12("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C")("D") must_===("D123456789ABC")
    A.ap13("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D")("E") must_===("E123456789ABCD")
    A.ap14("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E")("F") must_===("F123456789ABCDE")
    A.ap15("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F")("G") must_===("G123456789ABCDEF")
    A.ap16("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G")("H") must_===("H123456789ABCDEFG")
    A.ap17("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H")("I") must_===("I123456789ABCDEFGH")
    A.ap18("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I")("J") must_===("J123456789ABCDEFGHI")
    A.ap19("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J")("K") must_===("K123456789ABCDEFGHIJ")
    A.ap20("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")("L") must_===("L123456789ABCDEFGHIJK")
    A.ap21("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")("M") must_===("M123456789ABCDEFGHIJKL")
    A.ap22("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")("N") must_===("N123456789ABCDEFGHIJKLM")

    A.apply2("1", "2")((a, b) => undefined) must_===("12")
    A.apply3("1", "2", "3")((a, b, c) => undefined) must_===("123")
    A.apply4("1", "2", "3", "4")((a, b, c, d) => undefined) must_===("1234")
    A.apply5("1", "2", "3", "4", "5")((a, b, c, d, e) => undefined) must_===("12345")
    A.apply6("1", "2", "3", "4", "5", "6")((a, b, c, d, e, f) => undefined) must_===("123456")
    A.apply7("1", "2", "3", "4", "5", "6", "7")((a, b, c, d, e, f, g) => undefined) must_===("1234567")
    A.apply8("1", "2", "3", "4", "5", "6", "7", "8")((a, b, c, d, e, f, g, h) => undefined) must_===("12345678")
    A.apply9("1", "2", "3", "4", "5", "6", "7", "8", "9")((a, b, c, d, e, f, g, h, i) => undefined) must_===("123456789")
    A.apply10("1", "2", "3", "4", "5", "6", "7", "8", "9", "A")((a, b, c, d, e, f, g, h, i, j) => undefined) must_===("123456789A")
    A.apply11("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B")((a, b, c, d, e, f, g, h, i, j, k) => undefined) must_===("123456789AB")
    A.apply12("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C")((a, b, c, d, e, f, g, h, i, j, k, l) => undefined) must_===("123456789ABC")
    A.apply13("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D")((a, b, c, d, e, f, g, h, i, j, k, l, m) => undefined) must_===("123456789ABCD")
    A.apply14("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E")((a, b, c, d, e, f, g, h, i, j, k, l, m, n) => undefined) must_===("123456789ABCDE")
    A.apply15("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F")((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => undefined) must_===("123456789ABCDEF")
    A.apply16("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G")((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) => undefined) must_===("123456789ABCDEFG")
    A.apply17("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H")((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) => undefined) must_===("123456789ABCDEFGH")
    A.apply18("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I")((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) => undefined) must_===("123456789ABCDEFGHI")
    A.apply19("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J")((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) => undefined) must_===("123456789ABCDEFGHIJ")
    A.apply20("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) => undefined) must_===("123456789ABCDEFGHIJK")
    A.apply21("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) => undefined) must_===("123456789ABCDEFGHIJKL")
    A.apply22("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M")((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) => undefined) must_===("123456789ABCDEFGHIJKLM")
  }

  "apN" in {
    Apply[Option].ap2(some("1"), some("2"))(some((_: String) + (_: String))) must_===(some("12"))
    Apply[Option].ap3(some("1"), some("2"), some("3"))(some((_: String) + (_: String) + (_: String))) must_===(some("123"))
    Apply[Option].ap4(some("1"), some("2"), some("3"), some("4"))(some((_: String) + (_: String) + (_: String) + (_: String))) must_===(some("1234"))
    Apply[Option].ap5(some("1"), some("2"), some("3"), some("4"), some("5"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("12345"))
    Apply[Option].ap6(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456"))
    Apply[Option].ap7(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("1234567"))
    Apply[Option].ap8(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("12345678"))
    Apply[Option].ap9(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789"))
    Apply[Option].ap10(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789A"))
    Apply[Option].ap11(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789AB"))
    Apply[Option].ap12(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABC"))
    Apply[Option].ap13(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABCD"))
    Apply[Option].ap14(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABCDE"))
    Apply[Option].ap15(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABCDEF"))
    Apply[Option].ap16(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABCDEFG"))
    Apply[Option].ap17(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABCDEFGH"))
    Apply[Option].ap18(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABCDEFGHI"))
    Apply[Option].ap19(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"), some("J"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABCDEFGHIJ"))
    Apply[Option].ap20(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"), some("J"), some("K"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABCDEFGHIJK"))
    Apply[Option].ap21(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"), some("J"), some("K"), some("L"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABCDEFGHIJKL"))
    Apply[Option].ap22(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"), some("J"), some("K"), some("L"), some("M"))(some((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String))) must_===(some("123456789ABCDEFGHIJKLM"))
  }

  "<*>" in {
    some(9) <*> some({(_: Int) + 3}) must_===(some(12))
  }

  "have syntax" in {
    import syntax.apply._
    (some("1") ⊛ some("2") ⊛ some("3") ⊛ some("4") ⊛ some("5") ⊛ some("6") ⊛ some("7") ⊛ some("8") ⊛ some("9") ⊛ some("A") ⊛ some("B") ⊛ some("C") ⊛ some("D") ⊛ some("E") ⊛ some("F") ⊛ some("G") ⊛ some("H") ⊛ some("I") ⊛ some("J") ⊛ some("K") ⊛ some("L") ⊛ some("M"))((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String)) must_===(some("123456789ABCDEFGHIJKLM"))
    ^^^^^^^^^^^^^^^^^^^^^(some("1"), some("2"), some("3"), some("4"), some("5"), some("6"), some("7"), some("8"), some("9"), some("A"), some("B"), some("C"), some("D"), some("E"), some("F"), some("G"), some("H"), some("I"), some("J"), some("K"), some("L"), some("M"))((_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String) + (_: String)) must_===(some("123456789ABCDEFGHIJKLM"))
  }

  import Scalaz.none

  def err: Option[String] = sys.error("should be non-strict!")

  "*>" in {
    some(1)       *> some(2)   must_=== (some(2))
    some(1)       *> none[Int] must_=== (none[Int])
    none[Int]     *> none[Int] must_=== (none[Int])
    none[Int]     *> some(2)   must_=== (none[Int])
    (none[String] *> err).mustThrowA[RuntimeException]
  }

  "`*>ByName`" in {
    some(1)       `*>ByName` some(2)   must_=== (some(2))
    some(1)       `*>ByName` none[Int] must_=== (none[Int])
    none[Int]     `*>ByName` none[Int] must_=== (none[Int])
    none[Int]     `*>ByName` some(2)   must_=== (none[Int])
    none[String]  `*>ByName` err       must_=== (none[String])
  }

  "<*" in {
    some(1)       <* some(2)   must_=== (some(1))
    some(1)       <* none[Int] must_=== (none[Int])
    none[Int]     <* none[Int] must_=== (none[Int])
    none[Int]     <* some(2)   must_=== (none[Int])
    (none[String] <* err).mustThrowA[RuntimeException]
  }

  "<*ByName" in {
    some(1)       `<*ByName` some(2)   must_=== (some(1))
    some(1)       `<*ByName` none[Int] must_=== (none[Int])
    none[Int]     `<*ByName` none[Int] must_=== (none[Int])
    none[Int]     `<*ByName` some(2)   must_=== (none[Int])
    (none[String] `<*ByName` err)      must_=== (none[String])
  }

  def unfoldrOptShortCircuiting[F[_]](empty: F[Int])(implicit F: Applicative[F]): Unit = {
    val reducer: Reducer[F[Int], F[Int]] =
      F.liftReducer(Reducer.identityReducer[Int])

    val f: Int => Maybe[(F[Int], Int)] = i => {
      if (i > 0) Maybe.just((F.point(i), i - 1))
      else if (i == 0) Maybe.just((empty, i - 1))
      else sys.error("BOOM!")
    }

    reducer.unfoldrOpt(5)(f) must_== Maybe.just(empty)
  }

}
