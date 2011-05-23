package scalaz

trait Foldr[F[_]] {
  def foldr[A, B]: (A => (=> B) => B) => B => F[A] => B

  def foldr1[A]: (A => (=> A) => A) => F[A] => Option[A] =
    f => foldr[A, Option[A]](a => o => o.map(f(a)(_)))(None: Option[A])

  def foldMap[A, M](f: A => M)(implicit m: Monoid[M]): F[A] => M =
    foldr[A, M](a => b => m.append(f(a), b))(m.z)
}

object Foldr extends Foldrs

trait Foldrs extends FoldrsLow {

  import data.Endo

  def foldMapFoldr[F[_] : FoldMap]: Foldr[F] = new Foldr[F] {
    def foldr[A, B]: (A => (=> B) => B) => B => F[A] => B =
      k => b => as => {
        val m = implicitly[FoldMap[F]].foldMap((a: A) => Endo.endo[B](k(a)))
        val e = m(as)
        e(b)
      }
  }

  implicit val OptionFoldr: Foldr[Option] = new Foldr[Option] {
    def foldr[A, B] =
      f => z => {
        case None => z
        case Some(a) => f(a)(z)
      }
  }

  implicit val ListFoldr: Foldr[List] = new Foldr[List] {
    def foldr[A, B] = k => b => _.foldRight(b)((a, b) => k(a)(b))
  }

  implicit val StreamFoldr: Foldr[Stream] = new Foldr[Stream] {
    def foldr[A, B] = k => b => s =>
      if (s.isEmpty)
        b
      else
        k(s.head)(foldr(k)(b)(s.tail))
  }
}

trait FoldrsLow {
  implicit def TraversableFoldr[CC[X] <: Traversable[X]]: Foldr[CC] = new Foldr[CC] {
    def foldr[A, B] = k => b =>
      _.foldRight(b)((a, b) => k(a)(b))
   }
}
