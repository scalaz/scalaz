package scalaz
package benchmarks

trait LensModel {
  case class O0(o: O1)
  case class O1(o: O2)
  case class O2(o: O3)
  case class O3(o: O4)
  case class O4(o: O5)
  case class O5(o: O6)
  case class O6(o: O7)
  case class O7(o: O8)
  case class O8(o: O9)
  case class O9(i: Int)

  def oi(i: Int): O0 = O0(O1(O2(O3(O4(O5(O6(O7(O8(O9(i))))))))))
}
