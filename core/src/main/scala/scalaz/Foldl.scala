package scalaz

trait Foldl[F[_]] {
  def foldl[A, B]: (B => A => B) => B => F[A] => B

  def foldl1[A]: (A => A => A) => F[A] => Option[A] =
    f => foldl[A, Option[A]](o => a => o.map(f(a)(_)) orElse Some(a))(None: Option[A])

  def foldMap[A, M](f: A => M)(implicit m: Monoid[M]): F[A] => M =
    foldl[A, M](b => a => m.append(f(a), b))(m.z)

  def foldU[M](a: F[M])(implicit m: Monoid[M]): M = {
    val k = foldMap[M, M](z => z)
    k(a)
  }

  def deriving[G[_]](implicit n: ^**^[G, F]): Foldl[G] =
    new Foldl[G] {
      def foldl[A, B]: (B => A => B) => B => G[A] => B =
        k => b => z =>
          Foldl.this.foldl(k)(b)(n.unpack(z))
    }
}

object Foldl extends Foldls

trait Foldls extends FoldlsLow {
  implicit val OptionFoldl: Foldl[Option] = new Foldl[Option] {
    def foldl[A, B] = k => b => {
      case None => b
      case Some(a) => k(b)(a)
    }
  }

  implicit val ListFoldl: Foldl[List] = new Foldl[List] {
    def foldl[A, B] = k => b => _.foldLeft(b)((b, a) => k(b)(a))
  }

  implicit val StreamFoldl: Foldl[Stream] = new Foldl[Stream] {
    def foldl[A, B] = k => b => _.foldLeft(b)((b, a) => k(b)(a))
  }

  implicit def EitherFoldl[X]: Foldl[({type λ[α] = Either[X, α]})#λ] = new Foldl[({type λ[α] = Either[X, α]})#λ] {
    def foldl[A, B] =
      f => z => {
        case Left(_) => z
        case Right(a) => f(z)(a)
      }
  }

  implicit def LeftEitherFoldl[X]: Foldl[({type λ[α] = Either.LeftProjection[α, X]})#λ] = new Foldl[({type λ[α] = Either.LeftProjection[α, X]})#λ] {
    def foldl[A, B] =
      f => z => _.e match {
        case Left(a) => f(z)(a)
        case Right(_) => z
      }
  }

  implicit def RightEitherFoldl[X]: Foldl[({type λ[α] = Either.RightProjection[X, α]})#λ] = new Foldl[({type λ[α] = Either.RightProjection[X, α]})#λ] {
    def foldl[A, B] =
      f => z => _.e match {
        case Right(a) => f(z)(a)
        case Left(_) => z
      }
  }

  implicit val NonEmptyListFoldl: Foldl[NonEmptyList] = new Foldl[NonEmptyList] {
    def foldl[A, B] = k => b => _.list.foldLeft(b)((b, a) => k(b)(a))
  }

  implicit def OptionTFoldl[F[_]: Foldl]: Foldl[({type λ[α] = OptionT[F, α]})#λ] = new Foldl[({type λ[α] = OptionT[F, α]})#λ] {
    def foldl[A, B] = k => b => s =>  {
      implicitly[Foldl[F]].foldl[Option[A], B](b => a => implicitly[Foldl[Option]].foldl(k)(b)(a))(b)(s.runT)
    }
  }

  implicit def LazyOptionTFoldl[F[_]: Foldl]: Foldl[({type λ[α] = LazyOptionT[F, α]})#λ] = new Foldl[({type λ[α] = LazyOptionT[F, α]})#λ] {
    def foldl[A, B] = k => b => s =>  {
      implicitly[Foldl[F]].foldl[LazyOption[A], B](b => a => implicitly[Foldl[LazyOption]].foldl(k)(b)(a))(b)(s.runT)
    }
  }

  implicit def EitherTFoldl[F[_]: Foldl, X]: Foldl[({type λ[α] = EitherT[X, F, α]})#λ] = new Foldl[({type λ[α] = EitherT[X, F, α]})#λ] {
    def foldl[A, B] = k => b => s =>  {
      implicitly[Foldl[F]].foldl[Either[X, A], B](b => a => implicitly[Foldl[({type λ[α] = Either[X, α]})#λ]].foldl(k)(b)(a))(b)(s.runT)
    }
  }

  implicit def LeftEitherTFoldl[F[_]: Foldl, X]: Foldl[({type λ[α] = EitherT.LeftProjectionT[α, F, X]})#λ] = new Foldl[({type λ[α] = EitherT.LeftProjectionT[α, F, X]})#λ] {
    def foldl[A, B] = k => b => s =>  {
      implicitly[Foldl[F]].foldl[Either[A, X], B](b => a => implicitly[Foldl[({type λ[α] = Either.LeftProjection[α, X]})#λ]].foldl(k)(b)(a.left))(b)(s.e.runT)
    }
  }

  implicit def LazyEitherTFoldl[F[_]: Foldl, X]: Foldl[({type λ[α] = LazyEitherT[X, F, α]})#λ] = new Foldl[({type λ[α] = LazyEitherT[X, F, α]})#λ] {
    def foldl[A, B] = k => b => s =>  {
      implicitly[Foldl[F]].foldl[LazyEither[X, A], B](b => a => implicitly[Foldl[({type λ[α] = LazyEither[X, α]})#λ]].foldl(k)(b)(a))(b)(s.runT)
    }
  }

  implicit def LazyLeftEitherTFoldl[F[_]: Foldl, X]: Foldl[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, X]})#λ] = new Foldl[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, X]})#λ] {
    def foldl[A, B] = k => b => s =>  {
      implicitly[Foldl[F]].foldl[LazyEither[A, X], B](b => a => implicitly[Foldl[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ]].foldl(k)(b)(a.left))(b)(s.e.runT)
    }
  }

  implicit val LazyOptionFoldl: Foldl[LazyOption] = new Foldl[LazyOption] {
    def foldl[A, B] =
      f => z => _.fold(
        some = a => f(z)(a)
      , none = z
      )
  }

  implicit def LazyEitherFoldl[X]: Foldl[({type λ[α] = LazyEither[X, α]})#λ] = new Foldl[({type λ[α] = LazyEither[X, α]})#λ] {
    def foldl[A, B] =
      f => z => _.fold(
        left  = _ => z
      , right = a => f(z)(a)
      )
  }

  implicit def LazyLeftEitherFoldl[X]: Foldl[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ] = new Foldl[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ] {
    def foldl[A, B] =
      f => z => _.e.fold(
        left  = a => f(z)(a)
      , right = _ => z
      )
  }

}


trait FoldlsLow {
  implicit def TraversableFoldl[CC[X] <: Traversable[X]]: Foldl[CC] = new Foldl[CC] {
    def foldl[A, B] = k => b =>
      _.foldLeft(b)((a, b) => k(a)(b))
  }
}
