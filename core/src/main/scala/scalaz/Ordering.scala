package scalaz

sealed trait Ordering {
  val toInt: Int
}
case object LT extends Ordering {
  val toInt = -1
}
case object EQ extends Ordering {
  val toInt = 0
}
case object GT extends Ordering {
  val toInt = 1
}
