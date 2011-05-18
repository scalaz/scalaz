package scalaz

trait Copure[-C[_]] {
  def copure[A](p: C[A]): A
}

object Copure {
  import Scalaz._
  
  implicit def IdentityCopure: Copure[Identity] = new Copure[Identity] {
    def copure[A](a: Identity[A]) = a
  }

  implicit def NonEmptyListCopure: Copure[NonEmptyList] = new Copure[NonEmptyList] {
    def copure[A](a: NonEmptyList[A]) = a.head
  }

  implicit def ZeroCopure: Copure[Zero] = new Copure[Zero] {
    def copure[A](a: Zero[A]) = a.zero
  }

  implicit def Tuple1Copure: Copure[Tuple1] = new Copure[Tuple1] {
    def copure[A](a: Tuple1[A]) = a._1
  }

  implicit def Tuple2Copure[R]: Copure[({type λ[α]=(R, α)})#λ] = new Copure[({type λ[α]=(R, α)})#λ] {
    def copure[A](a: (R, A)) = a._2
  }

  implicit def Function0Copure: Copure[Function0] = new Copure[Function0] {
    def copure[A](a: () => A) = a.apply
  }

  import java.util.concurrent.Callable

  implicit def CallableCopure: Copure[Callable] = new Copure[Callable] {
    def copure[A](a: Callable[A]) = a.call
  }

  import java.util.Map.Entry

  implicit def MapEntryCopure[X]: Copure[({type λ[α]=Entry[X, α]})#λ] = new Copure[({type λ[α]=Entry[X, α]})#λ] {
    def copure[A](a: Entry[X, A]) = a.getValue
  }

  implicit def ZipperCopure: Copure[Zipper] = new Copure[Zipper] {
    def copure[A](a: Zipper[A]) = a.focus
  }

  implicit def TreeCopure: Copure[Tree] = new Copure[Tree] {
    def copure[A](a: Tree[A]) = a.rootLabel
  }
  implicit def TreeLocCopure: Copure[TreeLoc] = new Copure[TreeLoc] {
    def copure[A](a: TreeLoc[A]) = a.tree.rootLabel
  }

  import concurrent.Promise
  implicit def PromiseCopure: Copure[Promise] = new Copure[Promise] {
    def copure[A](a: Promise[A]) = a.get
  }
}
