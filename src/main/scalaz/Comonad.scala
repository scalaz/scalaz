package scalaz

trait Comonad[M[_]] extends Copointed[M] with Cojoin[M]

object Comonad {
  def comonad[M[_]](implicit p: Copointed[M], j: Cojoin[M]) = new Comonad[M] {
    def fmap[A, B](fa: M[A], f: A => B) = p.fmap(fa, f)
    def copure[A](a: M[A]) = p.copure(a)
    def cobind[A, B](a: M[A], f: M[A] => B) = p.fmap(cojoin(a), f)
    def cojoin[A](a: M[A]) = j.cojoin(a)
  }
}