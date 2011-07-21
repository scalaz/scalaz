package scalaz

trait Foldr[F[_]] {
  def foldr[A, B]: (A => (=> B) => B) => B => F[A] => B

  def foldr1[A]: (A => (=> A) => A) => F[A] => Option[A] =
    f => foldr[A, Option[A]](a => o => o.map(f(a)(_)) orElse Some(a))(None: Option[A])

  def foldMap[A, M](f: A => M)(implicit m: Monoid[M]): F[A] => M =
    foldr[A, M](a => b => m.append(f(a), b))(m.z)

  def foldU[M](a: F[M])(implicit m: Monoid[M]): M = {
    val k = foldMap[M, M](z => z)
    k(a)
  }

  def deriving[G[_]](implicit n: ^**^[G, F]): Foldr[G] =
    new Foldr[G] {
      def foldr[A, B]: (A => (=> B) => B) => B => G[A] => B =
        k => b => z =>
          Foldr.this.foldr(k)(b)(n.unpack(z))
    }
}

object Foldr extends Foldrs

trait Foldrs extends FoldrsLow {
  implicit val OptionFoldr: Foldr[Option] = new Foldr[Option] {
    def foldr[A, B] =
      f => z => {
        case None => z
        case Some(a) => f(a)(z)
      }
  }

  implicit val ListFoldr: Foldr[List] = new Foldr[List] {
    def foldr[A, B] = k => b => _.foldRight(b)((a, b) => k(a)(b))
  }

  implicit val StreamFoldr: Foldr[Stream] = new Foldr[Stream] {
    def foldr[A, B] = k => b => s =>
      if (s.isEmpty)
        b
      else
        k(s.head)(foldr(k)(b)(s.tail))
  }

  implicit def EitherFoldr[X]: Foldr[({type λ[α] = Either[X, α]})#λ] = new Foldr[({type λ[α] = Either[X, α]})#λ] {
    def foldr[A, B] =
      f => z => {
        case Left(_) => z
        case Right(a) => f(a)(z)
      }
  }

  implicit def LeftEitherFoldr[X]: Foldr[({type λ[α] = Either.LeftProjection[α, X]})#λ] = new Foldr[({type λ[α] = Either.LeftProjection[α, X]})#λ] {
    def foldr[A, B] =
      f => z => _.e match {
        case Left(a) => f(a)(z)
        case Right(_) => z
      }
  }

  implicit def RightEitherFoldr[X]: Foldr[({type λ[α] = Either.RightProjection[X, α]})#λ] = new Foldr[({type λ[α] = Either.RightProjection[X, α]})#λ] {
    def foldr[A, B] =
      f => z => _.e match {
        case Right(a) => f(a)(z)
        case Left(_) => z
      }
  }

  implicit val NonEmptyListFoldr: Foldr[NonEmptyList] = new Foldr[NonEmptyList] {
    def foldr[A, B] = k => b => _.list.foldRight(b)((b, a) => k(b)(a))
  }

  implicit def OptionTFoldr[F[_]: Foldr]: Foldr[({type λ[α] = OptionT[F, α]})#λ] = new Foldr[({type λ[α] = OptionT[F, α]})#λ] {
    def foldr[A, B] = k => b => s =>  {
      implicitly[Foldr[F]].foldr[Option[A], B](a => b => implicitly[Foldr[Option]].foldr(k)(b)(a))(b)(s.runT)
    }
  }

  implicit def LazyOptionTFoldr[F[_]: Foldr]: Foldr[({type λ[α] = LazyOptionT[F, α]})#λ] = new Foldr[({type λ[α] = LazyOptionT[F, α]})#λ] {
    def foldr[A, B] = k => b => s =>  {
      implicitly[Foldr[F]].foldr[LazyOption[A], B](a => b => implicitly[Foldr[LazyOption]].foldr(k)(b)(a))(b)(s.runT)
    }
  }

  implicit def EitherTFoldr[F[_]: Foldr, X]: Foldr[({type λ[α] = EitherT[X, F, α]})#λ] = new Foldr[({type λ[α] = EitherT[X, F, α]})#λ] {
    def foldr[A, B] = k => b => s =>  {
      implicitly[Foldr[F]].foldr[Either[X, A], B](a => b => implicitly[Foldr[({type λ[α] = Either[X, α]})#λ]].foldr(k)(b)(a))(b)(s.runT)
    }
  }

  implicit def LeftEitherTFoldr[F[_]: Foldr, X]: Foldr[({type λ[α] = EitherT.LeftProjectionT[α, F, X]})#λ] = new Foldr[({type λ[α] = EitherT.LeftProjectionT[α, F, X]})#λ] {
    def foldr[A, B] = k => b => s =>  {
      implicitly[Foldr[F]].foldr[Either[A, X], B](a => b => implicitly[Foldr[({type λ[α] = Either.LeftProjection[α, X]})#λ]].foldr(k)(b)(a.left))(b)(s.e.runT)
    }
  }

  implicit def LazyEitherTFoldr[F[_]: Foldr, X]: Foldr[({type λ[α] = LazyEitherT[X, F, α]})#λ] = new Foldr[({type λ[α] = LazyEitherT[X, F, α]})#λ] {
    def foldr[A, B] = k => b => s =>  {
      implicitly[Foldr[F]].foldr[LazyEither[X, A], B](a => b => implicitly[Foldr[({type λ[α] = LazyEither[X, α]})#λ]].foldr(k)(b)(a))(b)(s.runT)
    }
  }

  implicit def LazyLeftEitherTFoldr[F[_]: Foldr, X]: Foldr[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, X]})#λ] = new Foldr[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, X]})#λ] {
    def foldr[A, B] = k => b => s =>  {
      implicitly[Foldr[F]].foldr[LazyEither[A, X], B](a => b => implicitly[Foldr[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ]].foldr(k)(b)(a.left))(b)(s.e.runT)
    }
  }

  implicit val LazyOptionFoldr: Foldr[LazyOption] = new Foldr[LazyOption] {
    def foldr[A, B] =
      f => z => _.fold(
        some = a => f(a)(z)
      , none = z
      )
  }

  implicit def LazyEitherFoldr[X]: Foldr[({type λ[α] = LazyEither[X, α]})#λ] = new Foldr[({type λ[α] = LazyEither[X, α]})#λ] {
    def foldr[A, B] =
      f => z => _.fold(
        left  = _ => z
      , right = a => f(a)(z)
      )
  }

  implicit def LazyLeftEitherFoldr[X]: Foldr[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ] = new Foldr[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ] {
    def foldr[A, B] =
      f => z => _.e.fold(
        left  = a => f(a)(z)
      , right = _ => z
      )
  }

  import scala.util.control.TailCalls
  import TailCalls.TailRec
  implicit def TailRecFoldl : Foldl[TailRec] = new Foldl[TailRec] {
    def foldl[A, B] =
      f => z => t => f(z)(t.result)
  }

  import scala.util.continuations.ControlContext
  implicit def ControlContextFoldl[B] : Foldl[({type T[A] = ControlContext[A,B,B]})#T] = new Foldl[({type T[A] = ControlContext[A,B,B]})#T] {
    def foldl[A, B] =
      f => z => t => f(z)(t.x)

  }

}

trait FoldrsLow {
  implicit def TraversableFoldr[CC[X] <: Traversable[X]]: Foldr[CC] = new Foldr[CC] {
    def foldr[A, B] = k => b =>
      _.foldRight(b)((a, b) => k(a)(b))
  }
}
