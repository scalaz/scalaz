package scalazdemo.control

/*
List(1, 2, 3) <+> List(4, 5, 6)
List(1, 2, 3, 4, 5, 6)

Some(7) <+> Some(8)
Some(7)

Some(7) <+> None
Some(7)

none[Int] <+> Some(8)
Some(8)

none[Int] <+> none
None
*/
object Plus {
  import scalaz.OptionW.none
  import scalaz.control.PlusW._
  
  val demoes = List(
    // <+>
    ("List(1, 2, 3) <+> List(4, 5, 6)", List(1, 2, 3) <+> List(4, 5, 6)),
    ("Some(7) <+> Some(8)", Some(7) <+> Some(8)),
    ("Some(7) <+> None", Some(7) <+> None),
    ("none[Int] <+> Some(8)", none[Int] <+> Some(8)),
    ("none[Int] <+> none", none[Int] <+> none)
  )
  
  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
