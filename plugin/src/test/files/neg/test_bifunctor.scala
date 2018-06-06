import scalaz.meta.minimal

@minimal("bimap", ("lmap", "rmap"))
trait Bifunctor[F[_, _]] {
  def bimap[A, B, C, D](fab: F[A, B])(ac: A => C, bd: B => D): F[C, D] =
    lmap(rmap(fab)(bd))(ac)
  def lmap[A, B, C](fab: F[A, B])(ac: A => C): F[C, B] =
    bimap(fab)(ac, identity)
  def rmap[A, B, D](fab: F[A, B])(bd: B => D): F[A, D] =
    bimap(fab)(identity, bd)
}

object test {
  val eitherInst: Bifunctor[Either] = new Bifunctor[Either] {} // bad

  val tuple2Inst: Bifunctor[Tuple2] = new Bifunctor[Tuple2] { // bad
    override def rmap[A, B, D](fab: (A, B))(bd: B => D) = ???
  }

  case class BiconstInt[A, B](i: Int)
  val biconstIntInst: Bifunctor[BiconstInt] = new Bifunctor[BiconstInt] { // ok
    override def bimap[A, B, C, D](fab: BiconstInt[A, B])(ac: A => C, bd: B => D) = ???
  }

  case class BiconstString[A, B](i: Int)
  val biconstStringInst: Bifunctor[BiconstString] = new Bifunctor[BiconstString] { // ok
    override def lmap[A, B, C](fab: BiconstString[A, B])(ac: A => C) = ???
    override def rmap[A, B, D](fab: BiconstString[A, B])(bd: B => D) = ???
  }
}
