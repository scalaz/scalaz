package scalaz

trait CoBind[F[_]] {
  def coBind[A, B](f: F[A] => B): F[A] => F[B]

  def deriving[G[_]](implicit n: ^**^[G, F]): CoBind[G] =
    new CoBind[G] {
      def coBind[A, B](f: G[A] => B) =
        h => n.pack(CoBind.this.coBind((i: F[A]) => f(n.pack(i)))(n.unpack(h)))
    }
}

object CoBind extends CoBinds

trait CoBinds {
  def coBind[F[_]](implicit e: Extend[F]): CoBind[F] = new CoBind[F] {
    def coBind[A, B](f: F[A] => B) =
      e.functor.fmap(f) compose e.coJoin.coJoin
  }

  implicit val ListCoBind: CoBind[List] =
    coBind[List]

  implicit val OptionCoBind: CoBind[Option] =
    coBind[Option]

  implicit def EitherLeftCoBind[X]: CoBind[({type λ[α] = Either.LeftProjection[α, X]})#λ] =
    coBind[({type λ[α] = Either.LeftProjection[α, X]})#λ]

  implicit def EitherRightCoBind[X]: CoBind[({type λ[α] = Either.RightProjection[X, α]})#λ] =
    coBind[({type λ[α] = Either.RightProjection[X, α]})#λ]

  implicit def EitherCoBind[X]: CoBind[({type λ[α] = Either[X, α]})#λ] =
    coBind[({type λ[α] = Either[X, α]})#λ]

  implicit def Function1CoBind[R: Semigroup]: CoBind[({type λ[α] = (R => α)})#λ] =
    coBind[({type λ[α] = (R => α)})#λ]

  import java.util.concurrent.Callable

  implicit def CallableCoBind: CoBind[Callable] =
    coBind[Callable]

  import java.util.Map.Entry

  implicit def MapEntryCoBind[X]: CoBind[({type λ[α] = Entry[X, α]})#λ] =
    coBind[({type λ[α] = Entry[X, α]})#λ]

  implicit def IdentityCoBind: CoBind[Identity] =
    coBind[Identity]

  implicit def CoStateCoBind[A, F[_] : CoBind]: CoBind[({type λ[α] = CoStateT[A, F, α]})#λ] = new CoBind[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def coBind[X, Y](f: CoStateT[A, F, X] => Y) =
      _ cobindT f
  }
}
