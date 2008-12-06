package scalazdemo.control

/*
List(1, 2, 3) |+| List(4, 5, 6)
List(1, 2, 3, 4, 5, 6)

"abc" |+| "def"
abcdef

some(7) |+| some(8)
some(7)

some(7) |+| none
some(7)

none[Int] |+| some(8)
some(8)

none |+| none
None

(f |+| g)(3)
List(1, 2, 3, 3)

(f |+| g)(36)
List(1, 2, 3, 4, 5, 6, 7, 8, 9, :, ;, <, =, >, ?, @, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, 6, 3)
*/
object Semigroup {
  import scalaz.control.SemigroupW._
  import scalaz.OptionW.{none, some}

  val f = (n: Int) => (1 to n).map(i => (i + 48).toChar).toList
  val g = (n: Int) => n.toString.reverse.toList
  
  val demoes = List(
    // |+|
    ("List(1, 2, 3) |+| List(4, 5, 6)", List(1, 2, 3) |+| List(4, 5, 6)),
    ("\"abc\" |+| \"def\"", "abc" |+| "def"),
    ("some(7) |+| some(8)", some(7) |+| some(8)),
    ("some(7) |+| none", some(7) |+| none),
    ("none[Int] |+| some(8)", none[Int] |+| some(8)),
    ("none |+| none", none |+| none),
    ("(f |+| g)(3)", (f |+| g)(3)),
    ("(f |+| g)(36)", (f |+| g)(36))
  )
  
  def main(args: Array[String]) {
    demoes.foreach { case (s, x) => {
      println(s)
      println(x)
      println
    } }
  }
}
