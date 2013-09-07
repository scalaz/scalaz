package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class CoproductTest extends Spec {

  checkAll(comonad.laws[({type λ[α]=Coproduct[NonEmptyList, Tree, α]})#λ])
  checkAll(traverse.laws[({type λ[α]=Coproduct[Option, List, α]})#λ])

  object instances {
    def functor[F[_]: Functor, G[_]: Functor] = Functor[({type λ[α]=Coproduct[F, G, α]})#λ]
    def foldable[F[_]: Foldable, G[_]: Foldable] = Foldable[({type λ[α]=Coproduct[F, G, α]})#λ]
    def contravariant[F[_]: Contravariant, G[_]: Contravariant] = Contravariant[({type λ[α]=Coproduct[F, G, α]})#λ]
    def cobind[F[_]: Cobind, G[_]: Cobind] = Cobind[({type λ[α]=Coproduct[F, G, α]})#λ]
    def traverse[F[_]: Traverse, G[_]: Traverse] = Traverse[({type λ[α]=Coproduct[F, G, α]})#λ]
    def comonad[F[_]: Comonad, G[_]: Comonad] = Comonad[({type λ[α]=Coproduct[F, G, α]})#λ]

    // checking absence of ambiguity
    def invariantfunctor[F[_]: Comonad: Traverse: Contravariant, G[_]: Comonad: Traverse: Contravariant] = InvariantFunctor[({type λ[α]=Coproduct[F, G, α]})#λ]
    def invariantfunctor[F[_]: Functor: Contravariant, G[_]: Functor: Contravariant] = InvariantFunctor[({type λ[α]=Coproduct[F, G, α]})#λ]
    def functor[F[_]: Comonad: Traverse, G[_]: Comonad: Traverse] = Functor[({type λ[α]=Coproduct[F, G, α]})#λ]
    def foldable[F[_]: Traverse, G[_]: Traverse] = Foldable[({type λ[α]=Coproduct[F, G, α]})#λ]
    def cobind[F[_]: Comonad, G[_]: Comonad] = Cobind[({type λ[α]=Coproduct[F, G, α]})#λ]
  }
}
