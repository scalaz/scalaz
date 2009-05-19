package scalaz

trait Cojoin[M[_]] {
  def cojoin[A](a: M[A]): M[M[A]]
}

object Cojoin {
  implicit val IdentityCojoin = new Cojoin[Identity] {
    def cojoin[A](a: Identity[A]) = Identity.IdentityTo(a)
  }

  implicit val NonEmptyListCojoin = new Cojoin[NonEmptyList] {
    def cojoin[A](a: NonEmptyList[A]) = a.tails
  }

  implicit val Tuple1Cojoin = new Cojoin[Tuple1] {
    def cojoin[A](a: Tuple1[A]) = Tuple1(a)
  }

  implicit def Tuple2Cojoin[R] = new Cojoin[PartialApply1Of2[Tuple2, R]#Apply] {
    def cojoin[A](a: (R, A)) = (a._1, a)
  }

  implicit val Function0Cojoin = new Cojoin[Function0] {
    def cojoin[A](a: Function0[A]) = () => a
  }

  implicit val ZipperCojoin = new Cojoin[Zipper] {
    def cojoin[A](a: Zipper[A]) = a.positions
  }

  implicit val TreeCojoin: Cojoin[Tree] = new Cojoin[Tree] {
    def cojoin[A](a: Tree[A]): Tree[Tree[A]] = a.cobind(identity(_))
  }

  import Scalaz._
  import MA._

  implicit val TreeLocCojoin: Cojoin[TreeLoc] = new Cojoin[TreeLoc] {
    def cojoin[A](a: TreeLoc[A]): TreeLoc[TreeLoc[A]] = {
      val lft = (_: TreeLoc[A]).left
      val rgt = (_: TreeLoc[A]).right
      val p = a.parent.unfold[Stream]((o: Option[TreeLoc[A]]) =>
          for (z <- o) yield ((uf(z, lft), z, uf(z, rgt)), z.parent))
      TreeLoc.loc(a.unfoldTree(dwn[A](_: TreeLoc[A])), uf(a, lft), uf(a, rgt), p)
    }

    private def uf[A](a: TreeLoc[A], f: TreeLoc[A] => Option[TreeLoc[A]]): Stream[Tree[TreeLoc[A]]] =
      f(a).unfold[Stream]((o: Option[TreeLoc[A]]) =>
          for (c <- o) yield (c.unfoldTree(dwn[A](_: TreeLoc[A])), f(c)))

    private def dwn[A](tz: TreeLoc[A]): (TreeLoc[A], () => Stream[TreeLoc[A]]) = {
      (tz, () => tz.firstChild.unfold[Stream]((o: Option[TreeLoc[A]]) =>
          for (c <- o) yield (c, c.right)))
    }
  }

}