package scalaz

trait CoMonadTrans[F[_[_], _]] {
  def lower[G[_] : Extend, A](a: F[G, A]): G[A]
}

object CoMonadTrans extends CoMonadTranss

trait CoMonadTranss {

}