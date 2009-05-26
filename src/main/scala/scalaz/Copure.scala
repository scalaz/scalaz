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

  implicit val ZipperCopure = new Copure[Zipper] {
    def copure[A](a: Zipper[A]) = a.focus
  }

  implicit val TreeCopure = new Copure[Tree] {
    def copure[A](a: Tree[A]) = a.rootLabel
  }

  implicit val TreeLocCopure = new Copure[TreeLoc] {
    def copure[A](a: TreeLoc[A]) = a.tree.rootLabel
  }

  import concurrent.Promise
  implicit val PromiseCopure = new Copure[Promise] {
    def copure[A](a: Promise[A]) = a.get
  }

}