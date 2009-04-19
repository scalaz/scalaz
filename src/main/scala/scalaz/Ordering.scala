package scalaz

sealed trait Ordering {
  val toInt: Int
}
final case object LT extends Ordering {
  val toInt = -1
}
final case object EQ extends Ordering {
  val toInt = 0
}
final case object GT extends Ordering {
  val toInt = 1
}
