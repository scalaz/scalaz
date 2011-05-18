package scalaz

trait Foldl[F[_]] {
  def foldl[A, B]: (B => A => B) => B => F[A] => B

  def foldl1[A]: (A => A => A) => F[A] => Option[A] =
    f => foldl[A, Option[A]](o => a => o.map(f(_)(a)))(None: Option[A])
}

object Foldl extends Foldls

trait Foldls {
  implicit val ListFoldl: Foldl[List] = new Foldl[List] {
    def foldl[A, B] = k => b => _.foldLeft(b)((b, a) => k(b)(a))
  }

  implicit val StreamFoldl: Foldl[Stream] = new Foldl[Stream] {
    def foldl[A, B] = k => b => _.foldLeft(b)((b, a) => k(b)(a))
  }
}
