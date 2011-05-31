package scalaz

trait Foldl[F[_]] {
  def foldl[A, B]: (B => A => B) => B => F[A] => B

  def foldl1[A]: (A => A => A) => F[A] => Option[A] =
    f => foldl[A, Option[A]](o => a => o.map(f(_)(a)))(None: Option[A])

  def deriving[G[_]](implicit n: ^**^[G, F]): Foldl[G] =
    new Foldl[G] {
      def foldl[A, B]: (B => A => B) => B => G[A] => B =
        k => b => z =>
          Foldl.this.foldl(k)(b)(n.unpack(z))
    }
}

object Foldl extends Foldls

trait Foldls extends FoldlsLow {
  implicit val OptionFoldl: Foldl[Option] = new Foldl[Option] {
    def foldl[A, B] = k => b => {
      case None => b
      case Some(a) => k(b)(a)
    }
  }

  implicit val ListFoldl: Foldl[List] = new Foldl[List] {
    def foldl[A, B] = k => b => _.foldLeft(b)((b, a) => k(b)(a))
  }

  implicit val StreamFoldl: Foldl[Stream] = new Foldl[Stream] {
    def foldl[A, B] = k => b => _.foldLeft(b)((b, a) => k(b)(a))
  }
}


trait FoldlsLow {
  implicit def TraversableFoldl[CC[X] <: Traversable[X]]: Foldl[CC] = new Foldl[CC] {
    def foldl[A, B] = k => b =>
      _.foldLeft(b)((a, b) => k(a)(b))
  }
}
