package scalaz
import org.scalacheck.Prop.forAll


object IdTTest extends SpecLite {

  object instances {
    def functor[F[_] : Functor] = Functor[IdT[F, ?]]
    def apply[F[_] : Apply] = Apply[IdT[F, ?]]
    def monad[F[_] : Monad] = Monad[IdT[F, ?]]
    def foldable[F[_] : Foldable] = Foldable[IdT[F, ?]]
    def traverse[F[_] : Traverse] = Traverse[IdT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[IdT[F, ?]]
    def functor[F[_] : Monad : Traverse] = Functor[IdT[F, ?]]
    def apply[F[_] : Monad] = Apply[IdT[F, ?]]
    def foldable[F[_] : Traverse] = Foldable[IdT[F, ?]]
  }
}
