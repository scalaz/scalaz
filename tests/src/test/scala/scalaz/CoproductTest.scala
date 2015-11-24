package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object CoproductTest extends SpecLite {

  checkAll(comonad.laws[Coproduct[NonEmptyList, Tree, ?]])
  checkAll(traverse.laws[Coproduct[Option, List, ?]])
  checkAll(traverse1.laws[Coproduct[NonEmptyList, NonEmptyList, ?]])

  object instances {
    def functor[F[_]: Functor, G[_]: Functor] = Functor[Coproduct[F, G, ?]]
    def foldable[F[_]: Foldable, G[_]: Foldable] = Foldable[Coproduct[F, G, ?]]
    def foldable1[F[_]: Foldable1, G[_]: Foldable1] = Foldable1[Coproduct[F, G, ?]]
    def contravariant[F[_]: Contravariant, G[_]: Contravariant] = Contravariant[Coproduct[F, G, ?]]
    def cobind[F[_]: Cobind, G[_]: Cobind] = Cobind[Coproduct[F, G, ?]]
    def traverse[F[_]: Traverse, G[_]: Traverse] = Traverse[Coproduct[F, G, ?]]
    def traverse1[F[_]: Traverse1, G[_]: Traverse1] = Traverse1[Coproduct[F, G, ?]]
    def comonad[F[_]: Comonad, G[_]: Comonad] = Comonad[Coproduct[F, G, ?]]

    // checking absence of ambiguity
    def invariantfunctor[F[_]: Comonad: Traverse: Contravariant, G[_]: Comonad: Traverse: Contravariant] = InvariantFunctor[Coproduct[F, G, ?]]
    def invariantfunctor[F[_]: Functor: Contravariant, G[_]: Functor: Contravariant] = InvariantFunctor[Coproduct[F, G, ?]]
    def functor[F[_]: Comonad: Traverse, G[_]: Comonad: Traverse] = Functor[Coproduct[F, G, ?]]
    def foldable[F[_]: Traverse, G[_]: Traverse] = Foldable[Coproduct[F, G, ?]]
    def foldable[F[_]: Traverse1, G[_]: Traverse1] = Foldable[Coproduct[F, G, ?]]
    def traverse[F[_]: Traverse1, G[_]: Traverse1] = Traverse[Coproduct[F, G, ?]]
    def foldable1[F[_]: Traverse1, G[_]: Traverse1] = Foldable1[Coproduct[F, G, ?]]
    def cobind[F[_]: Comonad, G[_]: Comonad] = Cobind[Coproduct[F, G, ?]]
  }
}
