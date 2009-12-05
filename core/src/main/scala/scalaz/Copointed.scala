package scalaz

trait Copointed[C[_]] extends Functor[C] with Copure[C]

object Copointed {
  implicit def copointed[C[_]](implicit t: Functor[C], c: Copure[C]): Copointed[C] = new Copointed[C] {
    def fmap[A, B](a: C[A], f: A => B) = t.fmap(a, f)
    def copure[A](a: C[A]): A = c.copure(a)
  }
}
