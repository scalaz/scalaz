package scalaz

trait CoJoin[M[_]] {
  def coJoin[A]: M[A] => M[M[A]]
}

object CoJoin extends CoJoins

trait CoJoins {
  implicit def Tuple1CoJoin: CoJoin[Tuple1] = new CoJoin[Tuple1] {
    def coJoin[A] = a => Tuple1(a)
  }

  implicit def Tuple2CoJoin[R]: CoJoin[({type λ[α] = (R, α)})#λ] = new CoJoin[({type λ[α] = (R, α)})#λ] {
    def coJoin[A] = a => (a._1, a)
  }

  implicit def Function0CoJoin: CoJoin[Function0] = new CoJoin[Function0] {
    def coJoin[A] = a => () => a
  }

  import java.util.concurrent.Callable

  implicit def CallableCoJoin: CoJoin[Callable] = new CoJoin[Callable] {
    def coJoin[A] = a => new Callable[Callable[A]] {
      def call = a
    }
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryCoJoin[X]: CoJoin[({type λ[α] = Entry[X, α]})#λ] = new CoJoin[({type λ[α] = Entry[X, α]})#λ] {
    def coJoin[A] = a => new SimpleImmutableEntry(a.getKey, a)
  }

  implicit def IdentityCoJoin: CoJoin[Identity] = new CoJoin[Identity] {
    def coJoin[A] = a => Identity.id(a)
  }

  implicit def CoStateCoJoin[A, F[_] : CoBind]: CoJoin[({type λ[α] = CoStateT[A, F, α]})#λ] = new CoJoin[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def coJoin[X] =
      _.duplicateT
  }

  implicit def NonEmptyListCoJoin: CoJoin[NonEmptyList] = new CoJoin[NonEmptyList] {
    def coJoin[A] = a => a.tails
  }

  implicit def TreeCoJoin: CoJoin[Tree] = new CoJoin[Tree] {
    def coJoin[A] = a => a.cobind(identity(_))
  }

  implicit def TreeLocCojoin: CoJoin[TreeLoc] = new CoJoin[TreeLoc] {

    import *._

    private def dwn[A](tz: TreeLoc[A]): (TreeLoc[A], () => Stream[TreeLoc[A]]) =
      (tz, () => tz.firstChild.unfold[Stream, TreeLoc[A]]((o: Option[TreeLoc[A]]) =>
        for (c <- o) yield (c, c.right)))

    private def uf[A](a: TreeLoc[A], f: TreeLoc[A] => Option[TreeLoc[A]]): Stream[Tree[TreeLoc[A]]] =
      f(a).unfold[Stream, Tree[TreeLoc[A]]]((o: Option[TreeLoc[A]]) =>
        for (c <- o) yield (c.unfoldTree(dwn[A](_: TreeLoc[A])), f(c)))

    def coJoin[A] =
      a => {
        val lft = (_: TreeLoc[A]).left
        val rgt = (_: TreeLoc[A]).right
        val p = a.parent.unfold[Stream, (Stream[Tree[TreeLoc[A]]], TreeLoc[A], Stream[Tree[TreeLoc[A]]])]((o: Option[TreeLoc[A]]) =>
          for (z <- o) yield ((uf(z, lft), z, uf(z, rgt)), z.parent))
        TreeLoc.loc(a.unfoldTree(dwn[A](_: TreeLoc[A])), uf(a, lft), uf(a, rgt), p)
      }
  }

  implicit def ZipperCoJoin: CoJoin[Zipper] = new CoJoin[Zipper] {
    def coJoin[A] = a => a.positions
  }

}
