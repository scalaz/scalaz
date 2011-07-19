package scalaz

trait Join[F[_]] {
  def join[A]: F[F[A]] => F[A]
}

object Join extends Joins

trait Joins {
  implicit val OptionJoin: Join[Option] = new Join[Option] {
    def join[A] =
      _ flatMap identity
  }

  implicit val ListJoin: Join[List] = new Join[List] {
    def join[A] =
      _ flatMap identity
  }

  implicit val StreamJoin: Join[Stream] = new Join[Stream] {
    def join[A] =
      _ flatMap identity
  }

  implicit def EitherLeftJoin[X]: Join[({type λ[α] = Either.LeftProjection[α, X]})#λ] = new Join[({type λ[α] = Either.LeftProjection[α, X]})#λ] {
    def join[A] =
      _ flatMap (_.e) left
  }

  implicit def EitherRightJoin[X]: Join[({type λ[α] = Either.RightProjection[X, α]})#λ] = new Join[({type λ[α] = Either.RightProjection[X, α]})#λ] {
    def join[A] =
      _ flatMap (_.e) right
  }

  implicit def EitherJoin[X]: Join[({type λ[α] = Either[X, α]})#λ] = new Join[({type λ[α] = Either[X, α]})#λ] {
    def join[A] =
      _.right flatMap identity
  }

  import java.util.Map.Entry

  implicit def MapEntryJoin[X: Semigroup]: Join[({type λ[α] = Entry[X, α]})#λ] = new Join[({type λ[α] = Entry[X, α]})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = Entry[X, α]})#λ]] bind identity
  }

  implicit def Tuple1Join[X]: Join[Tuple1] = new Join[Tuple1] {
    def join[A] =
      implicitly[Bind[Tuple1]] bind identity
  }

  implicit def Tuple2Join[R: Semigroup]: Join[({type λ[α] = (R, α)})#λ] = new Join[({type λ[α] = (R, α)})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, α)})#λ]] bind identity
  }

  implicit def Tuple3Join[R: Semigroup, S: Semigroup]: Join[({type λ[α] = (R, S, α)})#λ] = new Join[({type λ[α] = (R, S, α)})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, S, α)})#λ]] bind identity
  }

  implicit def Tuple4Join[R: Semigroup, S: Semigroup, T: Semigroup]: Join[({type λ[α] = (R, S, T, α)})#λ] = new Join[({type λ[α] = (R, S, T, α)})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, S, T, α)})#λ]] bind identity
  }

  implicit def Tuple5Join[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup]: Join[({type λ[α] = (R, S, T, U, α)})#λ] = new Join[({type λ[α] = (R, S, T, U, α)})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, S, T, U, α)})#λ]] bind identity
  }

  implicit def Tuple6Join[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup]: Join[({type λ[α] = (R, S, T, U, V, α)})#λ] = new Join[({type λ[α] = (R, S, T, U, V, α)})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, S, T, U, V, α)})#λ]] bind identity
  }

  implicit def Tuple7Join[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup, W: Semigroup]: Join[({type λ[α] = (R, S, T, U, V, W, α)})#λ] = new Join[({type λ[α] = (R, S, T, U, V, W, α)})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, S, T, U, V, W, α)})#λ]] bind identity
  }

  implicit def Function0Join[X]: Join[Function0] = new Join[Function0] {
    def join[A] =
      implicitly[Bind[Function0]] bind identity
  }

  implicit def Function1Join[R]: Join[({type λ[α] = (R => α)})#λ] = new Join[({type λ[α] = (R => α)})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R => α)})#λ]] bind identity
  }

  implicit def Function2Join[R, S]: Join[({type λ[α] = (R, S) => α})#λ] = new Join[({type λ[α] = (R, S) => α})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, S) => α})#λ]] bind identity
  }

  implicit def Function3Join[R, S, T]: Join[({type λ[α] = (R, S, T) => α})#λ] = new Join[({type λ[α] = (R, S, T) => α})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, S, T) => α})#λ]] bind identity
  }

  implicit def Function4Join[R, S, T, U]: Join[({type λ[α] = (R, S, T, U) => α})#λ] = new Join[({type λ[α] = (R, S, T, U) => α})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, S, T, U) => α})#λ]] bind identity
  }

  implicit def Function5Join[R, S, T, U, V]: Join[({type λ[α] = (R, S, T, U, V) => α})#λ] = new Join[({type λ[α] = (R, S, T, U, V) => α})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, S, T, U, V) => α})#λ]] bind identity
  }

  implicit def Function6Join[R, S, T, U, V, W]: Join[({type λ[α] = (R, S, T, U, V, W) => α})#λ] = new Join[({type λ[α] = (R, S, T, U, V, W) => α})#λ] {
    def join[A] =
      implicitly[Bind[({type λ[α] = (R, S, T, U, V, W) => α})#λ]] bind identity
  }

  implicit val ResponderJoin: Join[Responder] = new Join[Responder] {
    def join[A] =
      _ flatMap identity
  }

  import java.util._
  import java.util.concurrent._

  implicit def CallableJoin: Join[Callable] = new Join[Callable] {
    def join[A] =
      implicitly[Bind[Callable]] bind identity
  }

  implicit def JavaArrayListJoin: Join[ArrayList] = new Join[ArrayList] {
    def join[A] =
      implicitly[Bind[ArrayList]] bind identity
  }

  implicit def JavaLinkedListJoin: Join[LinkedList] = new Join[LinkedList] {
    def join[A] =
      implicitly[Bind[LinkedList]] bind identity
  }

  implicit def JavaPriorityQueueJoin: Join[PriorityQueue] = new Join[PriorityQueue] {
    def join[A] =
      implicitly[Bind[PriorityQueue]] bind identity
  }

  implicit def JavaStackJoin: Join[Stack] = new Join[Stack] {
    def join[A] =
      implicitly[Bind[Stack]] bind identity
  }

  implicit def JavaVectorJoin: Join[Vector] = new Join[Vector] {
    def join[A] =
      implicitly[Bind[Vector]] bind identity
  }

  implicit def JavaArrayBlockingQueueJoin: Join[ArrayBlockingQueue] = new Join[ArrayBlockingQueue] {
    def join[A] =
      implicitly[Bind[ArrayBlockingQueue]] bind identity
  }

  implicit def JavaConcurrentLinkedQueueJoin: Join[ConcurrentLinkedQueue] = new Join[ConcurrentLinkedQueue] {
    def join[A] =
      implicitly[Bind[ConcurrentLinkedQueue]] bind identity
  }

  implicit def JavaCopyOnWriteArrayListJoin: Join[CopyOnWriteArrayList] = new Join[CopyOnWriteArrayList] {
    def join[A] =
      implicitly[Bind[CopyOnWriteArrayList]] bind identity
  }

  implicit def JavaLinkedBlockingQueueJoin: Join[LinkedBlockingQueue] = new Join[LinkedBlockingQueue] {
    def join[A] =
      implicitly[Bind[LinkedBlockingQueue]] bind identity
  }

  implicit def JavaSynchronousQueueJoin: Join[SynchronousQueue] = new Join[SynchronousQueue] {
    def join[A] =
      implicitly[Bind[SynchronousQueue]] bind identity
  }

  implicit val IdentityJoin: Join[Identity] = implicitly[Monad[Identity]].join

  implicit def CoKleisliJoin[F[_], R]: Join[({type λ[α] = CoKleisli[R, F, α]})#λ] = new Join[({type λ[α] = CoKleisli[R, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def KleisliJoin[F[_], R](implicit bd: Bind[F]): Join[({type λ[α] = Kleisli[R, F, α]})#λ] = new Join[({type λ[α] = Kleisli[R, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit val NonEmptyListJoin: Join[NonEmptyList] = new Join[NonEmptyList] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def ReaderWriterStateTJoin[R, W: Semigroup, S, F[_] : BindFunctor]: Join[({type λ[α] = ReaderWriterStateT[R, W, S, F, α]})#λ] = new Join[({type λ[α] = ReaderWriterStateT[R, W, S, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def StateTJoin[A, F[_] : Bind]: Join[({type λ[α] = StateT[A, F, α]})#λ] = new Join[({type λ[α] = StateT[A, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def StepListTJoin[F[_] : Functor]: Join[({type λ[X] = StepListT[F, X]})#λ] = new Join[({type λ[X] = StepListT[F, X]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def StepStreamTJoin[F[_] : Functor]: Join[({type λ[X] = StepStreamT[F, X]})#λ] = new Join[({type λ[X] = StepStreamT[F, X]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def WriterTJoin[A: Semigroup, F[_] : BindFunctor]: Join[({type λ[α] = WriterT[A, F, α]})#λ] = new Join[({type λ[α] = WriterT[A, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def OptionTJoin[F[_] : Monad]: Join[({type λ[α] = OptionT[F, α]})#λ] = new Join[({type λ[α] = OptionT[F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def LazyOptionTJoin[F[_] : Monad]: Join[({type λ[α] = LazyOptionT[F, α]})#λ] = new Join[({type λ[α] = LazyOptionT[F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def EitherTJoin[F[_] : Monad, X]: Join[({type λ[α] = EitherT[X, F, α]})#λ] = new Join[({type λ[α] = EitherT[X, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def LeftEitherTJoin[F[_] : Monad, X]: Join[({type λ[α] = EitherT.LeftProjectionT[α, F, X]})#λ] = new Join[({type λ[α] = EitherT.LeftProjectionT[α, F, X]})#λ] {
    def join[A] =
      _ flatMap (_.e) left
  }

  implicit def LazyEitherTJoin[F[_] : Monad, X]: Join[({type λ[α] = LazyEitherT[X, F, α]})#λ] = new Join[({type λ[α] = LazyEitherT[X, F, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def LazyLeftEitherTJoin[F[_] : Monad, X]: Join[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, X]})#λ] = new Join[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, X]})#λ] {
    def join[A] =
      _ flatMap (_.e) left
  }

  implicit val LazyOptionJoin: Join[LazyOption] = new Join[LazyOption] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def LazyEitherJoin[X]: Join[({type λ[α] = LazyEither[X, α]})#λ] = new Join[({type λ[α] = LazyEither[X, α]})#λ] {
    def join[A] =
      _ flatMap (z => z)
  }

  implicit def LazyLeftEitherJoin[X]: Join[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ] = new Join[({type λ[α] = LazyEither.LazyLeftProjection[α, X]})#λ] {
    def join[A] =
      _ flatMap (_.e) left
  }
}
