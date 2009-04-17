package scalaz

trait Comonad[W[_]] extends Copointed[W] with Cojoin[W]

object Comonad {
  def comonad[W[_]] = new Comonad[W] {
    def cobind[A, B](a: W[A], f: W[A] => B) = fmap(a.cojoin, f)
  }
}
