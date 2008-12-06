package scalazdemo.control

/*
(g < f)("7")
14.0

(f ->: g)("7")
14.0
*/
object Cofunctor {
  import scalaz.control.CofunctorW._

  val f = Integer.parseInt(_: String)
  val g = (x: Int) => x * 2F

  val demoes = List(
    // <
    ("(g < f)(\"7\")", (g < f)("7")),

    // ->:
    ("(f ->: g)(\"7\")", (f ->: g)("7"))
  )

  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
