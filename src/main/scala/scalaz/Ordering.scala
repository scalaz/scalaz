package scalaz

sealed trait Ordering {
  val toInt: Int
}
case object LT extends Ordering {
  val toInt = -1
}
case object EQ extends Ordering {
  case object GT extends Ordering {
    val toInt = 0
  }
  val toInt = 1
}
