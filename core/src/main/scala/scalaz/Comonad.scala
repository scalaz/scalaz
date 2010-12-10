package scalaz

trait Comonad[W[_]] extends Copointed[W] with Cojoin[W]

trait ComonadLow {
  implicit def comonad[W[_]](implicit j: Cojoin[W], p: Copointed[W]): Comonad[W] = new Comonad[W] {
    def cojoin[A](a: W[A]) = j.cojoin(a)
    def fmap[A, B](a: W[A], f: A => B) = p.fmap(a, f)
    def copure[A](a: W[A]) = p.copure(a)
  }
}

object Comonad extends ComonadLow {
  import Cojoin._
  import Copointed._

  implicit def Tuple2Comonad[A] = comonad[({type λ[α]=(A, α)})#λ](Tuple2Cojoin, Tuple2Copointed)

  import java.util.Map.Entry

  implicit def MapEntryComonad[X] = comonad[({type λ[α]=Entry[X, α]})#λ](MapEntryCojoin, MapEntryCopointed)
}
