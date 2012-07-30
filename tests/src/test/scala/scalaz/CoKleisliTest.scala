package scalaz

class CoKleisliTest extends Spec {

  // TODO enforce laws.

  "compose" in {
    import std.AllInstances._

    val ck = CoKleisli((a: NonEmptyList[Int]) => a.size)
    val ck1 = ck compose ck
    val run: Int = ck1.run(NonEmptyList(0, 0))
    run must be_===(2)
  }

  object instances {
    def monad[F[_], W] = Monad[({type λ[α] = CoKleisli[F, W, α]})#λ]
    def arrId[F[_] : Copointed, W] = ArrId[({type λ[α, β] = CoKleisli[F, α, β]})#λ]
    def compose[F[_], W](implicit F: Cojoin[F] with Functor[F]) = Compose[({type λ[α, β] = CoKleisli[F, α, β]})#λ]
    def arrow[F[_] : Comonad, W] = Arrow[({type λ[α, β] = CoKleisli[F, α, β]})#λ]

    // checking absence of ambiguity
    def arrId[F[_] : Comonad, W] = ArrId[({type λ[α, β] = CoKleisli[F, α, β]})#λ]
    def compose[F[_], W](implicit F: Comonad[F]) = Compose[({type λ[α, β] = CoKleisli[F, α, β]})#λ]
  }

}
