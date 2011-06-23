package scalaz

trait CoMonadTrans[F[_[_], _]] {
  def lower[G[_] : Extend, A](a: F[G, A]): G[A]
}

object CoMonadTrans extends CoMonadTranss

trait CoMonadTranss {

  implicit def CoStateTCoMonadTrans[S]: CoMonadTrans[({type λ[α[_], β] = CoStateT[S, α, β]})#λ] = new CoMonadTrans[({type λ[α[_], β] = CoStateT[S, α, β]})#λ] {
    def lower[G[_] : Extend, A](a: CoStateT[S, G, A]) =
      implicitly[Extend[G]].fmap((z: S => A) => z(a.runT._2))(a.runT._1)
  }

}