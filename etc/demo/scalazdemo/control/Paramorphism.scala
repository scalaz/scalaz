package scalazdemo.control

/*
List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) |- even
(List(2, 6, 8),List(9, 6, 7, 3, 5, 8, 6, 9))

List(6, 7, 3, 5, 8, 6, 9) |- even
(List(6),List(7, 3, 5, 8, 6, 9))

List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) !- even
(List(),List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9))

List(7, 3, 5, 8, 6, 9) !- even
(List(7, 3, 5),List(8, 6, 9))

List(2, 4, 6, 1, 2, 3, 4, 5, 6) dropSelect even
List(1, 2, 3, 4, 5, 6)

Some(7) dropSelect even
Some(7)

Some(8) dropSelect even
None

none[Int] dropSelect even
None
*/
object ParamorphismW {
  import scalaz.control.ParamorphismW._
  import scalaz.OptionW.none  

  val even = ((_: Int) % 2 == 0)

  val demoes = List(
    // |-
    ("List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) |- even", List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) |- even),
    ("List(6, 7, 3, 5, 8, 6, 9) |- even", List(6, 7, 3, 5, 8, 6, 9) |- even),

    // !-
    ("List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) !- even", List(2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9) !- even),
    ("List(7, 3, 5, 8, 6, 9) !- even", List(7, 3, 5, 8, 6, 9) !- even),

    // dropSelect
    ("List(2, 4, 6, 1, 2, 3, 4, 5, 6) dropSelect even", List(2, 4, 6, 1, 2, 3, 4, 5, 6) dropSelect even),
    ("Some(7) dropSelect even", Some(7) dropSelect even),
    ("Some(8) dropSelect even", Some(8) dropSelect even),
    ("none[Int] dropSelect even", none[Int] dropSelect even)
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
