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
}