package scalaz

trait Copointed[C[_]] extends Functor[C] with Copure[C]

trait CopointedLow {
  implicit def copointed[C[_]](implicit t: Functor[C], c: Copure[C]): Copointed[C] = new Copointed[C] {
    def fmap[A, B](a: C[A], f: A => B) = t.fmap(a, f)

    def copure[A](a: C[A]): A = c.copure(a)
  }
}

object Copointed extends CopointedLow {
  import Functor._
  import Copure._

  implicit def Tuple2Copointed[A] = copointed[({type λ[α]=(A, α)})#λ](Tuple2Functor, Tuple2Copure)

  import java.util.Map.Entry
  implicit def MapEntryCopointed[X] = copointed[({type λ[α]=Entry[X, α]})#λ](MapEntryFunctor, MapEntryCopure)
}
