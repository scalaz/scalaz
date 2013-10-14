package scalaz
import org.scalacheck.Prop.forAll

object EndomorphicTest extends SpecLite {

  object instances{
    def semigroup[F[_, _]: Compose, A] = Semigroup[Endomorphic[F, A]]
    def monoid[F[_, _]: Category, A] = Monoid[Endomorphic[F, A]]

    def semigroup[F[_, _]: Category, A] = Semigroup[Endomorphic[F, A]]

    object kleisli {
      def semigroup[F[_]: Bind, A] = Semigroup[Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A]]
      def monoid[F[_]: Monad, A] = Monoid[Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A]]

      def semigroup[F[_]: Monad, A] = Semigroup[Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A]]
    }

    object cokleisli {
      def semigroup[F[_]: Cobind, A] = Semigroup[Endomorphic[({type λ[α, β] = Cokleisli[F, α, β]})#λ, A]]
      def monoid[F[_]: Comonad, A] = Monoid[Endomorphic[({type λ[α, β] = Cokleisli[F, α, β]})#λ, A]]

      def semigroup[F[_]: Comonad, A] = Semigroup[Endomorphic[({type λ[α, β] = Cokleisli[F, α, β]})#λ, A]]
    }
  }
}
