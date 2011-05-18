package scalaz

trait Cojoin[M[_]] {
  def cojoin[A](a: M[A]): M[M[A]]
}

object Cojoin {
  import Scalaz._
  
  implicit def IdentityCojoin: Cojoin[Identity] = new Cojoin[Identity] {
    def cojoin[A](a: Identity[A]) = a
  }

  implicit def NonEmptyListCojoin: Cojoin[NonEmptyList] = new Cojoin[NonEmptyList] {
    def cojoin[A](a: NonEmptyList[A]) = a.tails
  }

  implicit def Tuple1Cojoin: Cojoin[Tuple1] = new Cojoin[Tuple1] {
    def cojoin[A](a: Tuple1[A]) = Tuple1(a)
  }

  implicit def Tuple2Cojoin[R]: Cojoin[({type λ[α]=(R, α)})#λ] = new Cojoin[({type λ[α]=(R, α)})#λ] {
    def cojoin[A](a: (R, A)) = (a._1, a)
  }

  implicit def Function0Cojoin: Cojoin[Function0] = new Cojoin[Function0] {
    def cojoin[A](a: () => A) = () => a
  }

  import java.util.concurrent.Callable

  implicit def CallableCojoin: Cojoin[Callable] = new Cojoin[Callable] {
    def cojoin[A](a: Callable[A]) = new Callable[Callable[A]] {
      def call = a
    }
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryCojoin[X]: Cojoin[({type λ[α]=Entry[X, α]})#λ] = new Cojoin[({type λ[α]=Entry[X, α]})#λ] {
    def cojoin[A](a: Entry[X, A]) = new SimpleImmutableEntry(a.getKey, a)
  }

  implicit def ZipperCojoin: Cojoin[Zipper] = new Cojoin[Zipper] {
    def cojoin[A](a: Zipper[A]) = a.positions
  }

  implicit def TreeCojoin: Cojoin[Tree] = new Cojoin[Tree] {
    def cojoin[A](a: Tree[A]): Tree[Tree[A]] = a.cobind(identity(_))
  }

  implicit def TreeLocCojoin: Cojoin[TreeLoc] = new Cojoin[TreeLoc] {
    def cojoin[A](a: TreeLoc[A]): TreeLoc[TreeLoc[A]] = {
      val lft = (_: TreeLoc[A]).left
      val rgt = (_: TreeLoc[A]).right
      val p = a.parent.unfold[Stream, (Stream[Tree[TreeLoc[A]]], TreeLoc[A], Stream[Tree[TreeLoc[A]]])]((o: Option[TreeLoc[A]]) =>
          for (z <- o) yield ((uf(z, lft), z, uf(z, rgt)), z.parent))
      loc(a.unfoldTree(dwn[A](_: TreeLoc[A])), uf(a, lft), uf(a, rgt), p)
    }

    private def uf[A](a: TreeLoc[A], f: TreeLoc[A] => Option[TreeLoc[A]]): Stream[Tree[TreeLoc[A]]] =
      f(a).unfold[Stream, Tree[TreeLoc[A]]]((o: Option[TreeLoc[A]]) =>
          for (c <- o) yield (c.unfoldTree(dwn[A](_: TreeLoc[A])), f(c)))

    private def dwn[A](tz: TreeLoc[A]): (TreeLoc[A], () => Stream[TreeLoc[A]]) =
      (tz, () => tz.firstChild.unfold[Stream, TreeLoc[A]]((o: Option[TreeLoc[A]]) =>
          for (c <- o) yield (c, c.right)))
  }

  import concurrent.Promise
  implicit def PromiseCojoin: Cojoin[Promise] = new Cojoin[Promise] {
    def cojoin[A](a: Promise[A]) = promise(a)(a.strategy)
  }
}
