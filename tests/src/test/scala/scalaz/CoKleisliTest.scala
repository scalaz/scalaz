package scalaz

class CokleisliTest extends Spec {

  // TODO enforce laws.

  "compose" in {
    import std.AllInstances._

    val ck = Cokleisli((a: NonEmptyList[Int]) => a.size)
    val ck1 = ck compose ck
    val run: Int = ck1.run(NonEmptyList(0, 0))
    run must be_===(2)
  }

  object instances {
    def monad[F[_], W] = Monad[({type λ[α] = Cokleisli[F, W, α]})#λ]
    def compose[F[_], W](implicit F: Cobind[F]) = Compose[({type λ[α, β] = Cokleisli[F, α, β]})#λ]
    def arrow[F[_] : Comonad, W] = Arrow[({type λ[α, β] = Cokleisli[F, α, β]})#λ]

    // checking absence of ambiguity
    def compose[F[_], W](implicit F: Comonad[F]) = Compose[({type λ[α, β] = Cokleisli[F, α, β]})#λ]
  }

}
