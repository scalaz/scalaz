package scalaz


trait Comonad[W[_]] extends Copointed[W] with Cojoin[W] {
  def cobind[A, B](a: W[A], f: W[A] => B) : W[B] = fmap(cojoin(a), f)
}

object Comonad {
  def comonad[W[_]](implicit j: Cojoin[W], p: Copointed[W]) = new Comonad[W] {
    def cojoin[A](a: W[A]) = j.cojoin(a)
    def fmap[A, B](a: W[A], f: A => B) = p.fmap(a, f)
    def copure[A](a: W[A]) = p.copure(a)
  }

  implicit val IdentityComonad = comonad[Identity]

  implicit val NonEmptyListComonad = comonad[NonEmptyList]

  implicit val Tuple1Comonad = comonad[Tuple1]

  implicit def Tuple2Comonad[R] = comonad[PartialApply1Of2[Tuple2, R]#Apply]

  implicit val Function0Comonad = comonad[Function0]

  implicit val ZipperComonad = comonad[Zipper]

  implicit val TreeComonad = comonad[Tree]
}
