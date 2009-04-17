package scalaz

trait Comonad[W[_]] extends Copointed[W] with Cojoin[W]

object Comonad {
  def comonad[W[_]](implicit j: Cojoin[W], p: Copointed[W]) = new Comonad[W] {
    def cobind[A, B](a: W[A], f: W[A] => B) = fmap(cojoin(a), f)
    def cojoin[A](a: W[A]) = j.cojoin(a)
    def fmap[A, B](a: W[A], f: A => B) = p.fmap(a, f)
    def copure[A](a: W[A]) = p.copure(a)
  }
}
