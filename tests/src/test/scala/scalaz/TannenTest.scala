package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object TannenTest extends SpecLite {

  checkAll(traverse.laws[Tannen[IList, \/, Int, *]])
  checkAll(bitraverse.laws[Tannen[Maybe, Tuple2, *, *]])

  object instances {
    def functor[F[_]: Functor, G[_, _]: Bifunctor, E] = Functor[Tannen[F, G, E, *]]
    def foldable[F[_]: Foldable, G[_, _]: Bifoldable, E] = Foldable[Tannen[F, G, E, *]]
    def traverse[F[_]: Traverse, G[_, _]: Bitraverse, E] = Traverse[Tannen[F, G, E, *]]

    def bifunctor[F[_]: Functor, G[_, _]: Bifunctor] = Bifunctor[Tannen[F, G, *, *]]
    def bifoldable[F[_]: Foldable, G[_, _]: Bifoldable] = Bifoldable[Tannen[F, G, *, *]]
    def bitraverse[F[_]: Traverse, G[_, _]: Bitraverse] = Bitraverse[Tannen[F, G, *, *]]


    // checking absence of ambiguity
    def functor[F[_]: Traverse, G[_, _]: Bitraverse, E] = Functor[Tannen[F, G, E, *]]
    def foldable[F[_]: Traverse, G[_, _]: Bitraverse, E] = Foldable[Tannen[F, G, E, *]]

    def bifunctor[F[_]: Traverse, G[_, _]: Bitraverse] = Bifunctor[Tannen[F, G, *, *]]
    def bifoldable[F[_]: Traverse, G[_, _]: Bitraverse] = Bifoldable[Tannen[F, G, *, *]]
  }
}
