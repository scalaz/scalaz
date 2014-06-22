package scalaz

sealed trait NotNothing[A]

object NotNothing {
  implicit def isNotNothing[A]: NotNothing[A] = new NotNothing[A] {}
  implicit def isNothingAmb1: NotNothing[Nothing] = sys.error("ambiguous NotNothing implicit was used")
  implicit def isNothingAmb2: NotNothing[Nothing] = sys.error("ambiguous NotNothing implicit was used")
}
