package scalaz.example

import scalaz.data.Maybe2._

object Maybe2Usage extends App {

  val hi5 = just2("Hi", 5)

  hi5 match {
    case Just2(s, i) => println(s"$s $i!")
    case Empty2()    => println("""¯\_(ツ)_/¯""")
  }
}
