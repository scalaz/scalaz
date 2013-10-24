package scalaz
import org.scalacheck.Prop.forAll


object IdTTest extends SpecLite {

  object instances {
    def functor[F[_] : Functor] = Functor[({type λ[α] = IdT[F, α]})#λ]
    def apply[F[_] : Apply] = Apply[({type λ[α] = IdT[F, α]})#λ]
    def monad[F[_] : Monad] = Monad[({type λ[α] = IdT[F, α]})#λ]
    def foldable[F[_] : Foldable] = Foldable[({type λ[α] = IdT[F, α]})#λ]
    def traverse[F[_] : Traverse] = Traverse[({type λ[α] = IdT[F, α]})#λ]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[({type λ[α] = IdT[F, α]})#λ]
    def functor[F[_] : Monad : Traverse] = Functor[({type λ[α] = IdT[F, α]})#λ]
    def apply[F[_] : Monad] = Apply[({type λ[α] = IdT[F, α]})#λ]
    def foldable[F[_] : Traverse] = Foldable[({type λ[α] = IdT[F, α]})#λ]
  }
}
