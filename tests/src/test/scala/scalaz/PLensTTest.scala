package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.{Gen, Arbitrary}

class PLensTTest extends Spec {
  import PLensT._

  "list head" in {
    listHeadPLens[Int].get(List(1, 2)) must be_===(Some(1))
    listHeadPLens[Int].get(Nil) must be_===(None)
  }

  object instances {
    def arrId[F[_] : Pointed, G[_]: Pointed] = ArrId[({type λ[α, β]=PLensT[F, G, α, β]})#λ]
    def category[F[_] : Monad, G[_]: Monad] = Category[({type λ[α, β]=PLensT[F, G, α, β]})#λ]
    def choice[F[_] : Monad, G[_]: Monad] = Choice[({type λ[α, β]=PLensT[F, G, α, β]})#λ]
    def split[F[_] : Monad, G[_]: Monad] = Split[({type λ[α, β]=PLensT[F, G, α, β]})#λ]

    // checking absence of ambiguity
    def arrId[F[_] : Monad, G[_]: Monad] = ArrId[({type λ[α, β]=PLensT[F, G, α, β]})#λ]
  }

}
