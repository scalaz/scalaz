package scalaz

trait Copure[-C[_]] {
  def copure[A](p: C[A]): A
}

object Copure {
  implicit val IdentityCopure = new Copure[Identity] {
    def copure[A](a: Identity[A]) = a.value
  }

  implicit val NonEmptyListCopure = new Copure[NonEmptyList] {
    def copure[A](a: NonEmptyList[A]) = a.head
  }

  implicit val ZeroCopure = new Copure[Zero] {
    def copure[A](a: Zero[A]) = a.zero
  }

  implicit val Tuple1Copure = new Copure[Tuple1] {
    def copure[A](a: Tuple1[A]) = a._1
  }

  implicit def Tuple2Copure[R] = new Copure[PartialApply1Of2[Tuple2, R]#Apply] {
    def copure[A](a: Tuple2[R, A]) = a._2
  }

  implicit val Function0Copure = new Copure[Function0] {
    def copure[A](a: Function0[A]) = a.apply
  }

}