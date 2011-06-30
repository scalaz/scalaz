package scalaz

trait BindFunctor[F[_]] {
  val functor: Functor[F]
  val bind: Bind[F]

  import BindFunctor._

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def bind[A, B](f: A => F[B]): F[A] => F[B] =
    bind.bind(f)

  def deriving[G[_]](implicit n: ^**^[G, F]): BindFunctor[G] = {
    implicit val b: Bind[G] = bind.deriving[G]
    implicit val f: Functor[G] = functor.deriving[G]
    bindFunctor[G]
  }

}

object BindFunctor extends BindFunctors

trait BindFunctors extends BindFunctorsLow {
  def bindFunctor[F[_]](implicit f: Functor[F], b: Bind[F]): BindFunctor[F] = new BindFunctor[F] {
    val functor = f
    val bind = b
  }

  implicit val OptionBindFunctor: BindFunctor[Option] =
    bindFunctor

  implicit val ListBindFunctor: BindFunctor[List] =
    bindFunctor

  implicit val StreamBindFunctor: BindFunctor[Stream] =
    bindFunctor

  implicit def EitherLeftBindFunctor[X] =
    bindFunctor[({type λ[α] = Either.LeftProjection[α, X]})#λ]

  implicit def EitherRightBindFunctor[X] =
    bindFunctor[({type λ[α] = Either.RightProjection[X, α]})#λ]

  implicit def EitherBindFunctor[X] =
    bindFunctor[({type λ[α] = Either[X, α]})#λ]

  import java.util.Map.Entry

  implicit def MapEntryBindFunctor[X: Semigroup] =
    bindFunctor[({type λ[α] = Entry[X, α]})#λ]

  implicit def Tuple1BindFunctor = {
    bindFunctor[Tuple1]
  }

  implicit def Tuple2BindFunctor[R: Semigroup] = {
    bindFunctor[({type λ[α] = (R, α)})#λ]
  }

  implicit def Tuple3BindFunctor[R: Semigroup, S: Semigroup] = {
    bindFunctor[({type λ[α] = (R, S, α)})#λ]
  }

  implicit def Tuple4BindFunctor[R: Semigroup, S: Semigroup, T: Semigroup] = {
    bindFunctor[({type λ[α] = (R, S, T, α)})#λ]
  }

  implicit def Tuple5BindFunctor[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup] = {
    bindFunctor[({type λ[α] = (R, S, T, U, α)})#λ]
  }

  implicit def Tuple6BindFunctor[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup] = {
    bindFunctor[({type λ[α] = (R, S, T, U, V, α)})#λ]
  }

  implicit def Tuple7BindFunctor[R: Semigroup, S: Semigroup, T: Semigroup, U: Semigroup, V: Semigroup, W: Semigroup] = {
    bindFunctor[({type λ[α] = (R, S, T, U, V, W, α)})#λ]
  }

  implicit def Function0BindFunctor: BindFunctor[Function0] =
    bindFunctor[Function0]

  implicit def Function1BindFunctor[R]: BindFunctor[({type λ[α] = (R) => α})#λ] =
    bindFunctor[({type λ[α] = (R) => α})#λ]

  implicit def Function2BindFunctor[R, S]: BindFunctor[({type λ[α] = (R, S) => α})#λ] =
    bindFunctor[({type λ[α] = (R, S) => α})#λ]

  implicit def Function3BindFunctor[R, S, T]: BindFunctor[({type λ[α] = (R, S, T) => α})#λ] =
    bindFunctor[({type λ[α] = (R, S, T) => α})#λ]

  implicit def Function4BindFunctor[R, S, T, U]: BindFunctor[({type λ[α] = (R, S, T, U) => α})#λ] =
    bindFunctor[({type λ[α] = (R, S, T, U) => α})#λ]

  implicit def Function5BindFunctor[R, S, T, U, V]: BindFunctor[({type λ[α] = (R, S, T, U, V) => α})#λ] =
    bindFunctor[({type λ[α] = (R, S, T, U, V) => α})#λ]

  implicit def Function6BindFunctor[R, S, T, U, V, W]: BindFunctor[({type λ[α] = (R, S, T, U, V, W) => α})#λ] =
    bindFunctor[({type λ[α] = (R, S, T, U, V, W) => α})#λ]

  import java.util._
  import java.util.concurrent._

  implicit def CallableBindFunctor: BindFunctor[Callable] =
    bindFunctor[Callable]

  implicit def JavaArrayListBindFunctor: BindFunctor[ArrayList] =
    bindFunctor[ArrayList]

  implicit def JavaLinkedListBindFunctor: BindFunctor[LinkedList] =
    bindFunctor[LinkedList]

  implicit def JavaPriorityQueueBindFunctor: BindFunctor[PriorityQueue] =
    bindFunctor[PriorityQueue]

  implicit def JavaStackBindFunctor: BindFunctor[Stack] =
    bindFunctor[Stack]

  implicit def JavaVectorBindFunctor: BindFunctor[Vector] =
    bindFunctor[Vector]

  implicit def JavaArrayBlockingQueueBindFunctor: BindFunctor[ArrayBlockingQueue] =
    bindFunctor[ArrayBlockingQueue]

  implicit def JavaConcurrentLinkedQueueBindFunctor: BindFunctor[ConcurrentLinkedQueue] =
    bindFunctor[ConcurrentLinkedQueue]

  implicit def JavaCopyOnWriteArrayListBindFunctor: BindFunctor[CopyOnWriteArrayList] =
    bindFunctor[CopyOnWriteArrayList]

  implicit def JavaLinkedBlockingQueueBindFunctor: BindFunctor[LinkedBlockingQueue] =
    bindFunctor[LinkedBlockingQueue]

  implicit def JavaSynchronousQueueBindFunctor: BindFunctor[SynchronousQueue] =
    bindFunctor[SynchronousQueue]

  implicit val IdentityBindFunctor: BindFunctor[Identity] =
    BindFunctor.bindFunctor

  implicit def CoKleisliBindFunctor[A, F[_]]: BindFunctor[({type λ[α] = CoKleisli[A, F, α]})#λ] = new BindFunctor[({type λ[α] = CoKleisli[A, F, α]})#λ] {
    val functor = implicitly[Functor[({type λ[α] = CoKleisli[A, F, α]})#λ]]
    val bind = implicitly[Bind[({type λ[α] = CoKleisli[A, F, α]})#λ]]
  }

  implicit def KleisliBindFunctor[A, F[_] : BindFunctor]: BindFunctor[({type λ[α] = Kleisli[A, F, α]})#λ] = new BindFunctor[({type λ[α] = Kleisli[A, F, α]})#λ] {
    implicit val ftr = implicitly[BindFunctor[F]].functor
    implicit val b = implicitly[BindFunctor[F]].bind
    val functor = implicitly[Functor[({type λ[α] = Kleisli[A, F, α]})#λ]]
    val bind = implicitly[Bind[({type λ[α] = Kleisli[A, F, α]})#λ]]
  }

  implicit val NonEmptyListBindFunctor: BindFunctor[NonEmptyList] =
    bindFunctor

  implicit def StateTBindFunctor[A, F[_] : BindFunctor]: BindFunctor[({type λ[α] = StateT[A, F, α]})#λ] = new BindFunctor[({type λ[α] = StateT[A, F, α]})#λ] {
    implicit val ftr = implicitly[BindFunctor[F]].functor
    implicit val b = implicitly[BindFunctor[F]].bind
    val functor = implicitly[Functor[({type λ[α] = StateT[A, F, α]})#λ]]
    val bind = implicitly[Bind[({type λ[α] = StateT[A, F, α]})#λ]]
  }

  implicit def StepListTBindFunctor[F[_] : Functor]: BindFunctor[({type λ[α] = StepListT[F, α]})#λ] =
    bindFunctor[({type λ[α] = StepListT[F, α]})#λ]

  implicit def StepStreamTBindFunctor[F[_] : Functor]: BindFunctor[({type λ[α] = StepStreamT[F, α]})#λ] =
    bindFunctor[({type λ[α] = StepStreamT[F, α]})#λ]

  implicit val TreeBindFunctor: BindFunctor[Tree] =
    bindFunctor[Tree]

  implicit def FailProjectionBindFunctor[X]: BindFunctor[({type λ[α] = FailProjection[α, X]})#λ] =
    bindFunctor[({type λ[α] = FailProjection[α, X]})#λ]

  implicit def WriterTBindFunctor[A: Semigroup, F[_] : BindFunctor]: BindFunctor[({type λ[α] = WriterT[A, F, α]})#λ] = new BindFunctor[({type λ[α] = WriterT[A, F, α]})#λ] {
    implicit val ftr = implicitly[BindFunctor[F]].functor
    val functor = implicitly[Functor[({type λ[α] = WriterT[A, F, α]})#λ]]
    val bind = implicitly[Bind[({type λ[α] = WriterT[A, F, α]})#λ]]
  }

  implicit def OptionTBindFunctor[F[_] : Monad]: BindFunctor[({type λ[α] = OptionT[F, α]})#λ] = {
    implicit val b = implicitly[Monad[F]].bind
    implicit val f = implicitly[Monad[F]].functor
    bindFunctor[({type λ[α] = OptionT[F, α]})#λ]
  }

  implicit def LazyOptionTBindFunctor[F[_] : Monad]: BindFunctor[({type λ[α] = LazyOptionT[F, α]})#λ] = {
    implicit val b = implicitly[Monad[F]].bind
    implicit val f = implicitly[Monad[F]].functor
    bindFunctor[({type λ[α] = LazyOptionT[F, α]})#λ]
  }

  implicit def EitherTBindFunctor[F[_] : Monad, A]: BindFunctor[({type λ[α] = EitherT[A, F, α]})#λ] = {
    implicit val b = implicitly[Monad[F]].bind
    implicit val f = implicitly[Monad[F]].functor
    bindFunctor[({type λ[α] = EitherT[A, F, α]})#λ]
  }

  implicit def LeftEitherTBindFunctor[F[_] : Monad, B]: BindFunctor[({type λ[α] = EitherT.LeftProjectionT[α, F, B]})#λ] = {
    implicit val b = implicitly[Monad[F]].bind
    implicit val f = implicitly[Monad[F]].functor
    bindFunctor[({type λ[α] = EitherT.LeftProjectionT[α, F, B]})#λ]
  }

  implicit def LazyEitherTBindFunctor[F[_] : Monad, A]: BindFunctor[({type λ[α] = LazyEitherT[A, F, α]})#λ] = {
    implicit val b = implicitly[Monad[F]].bind
    implicit val f = implicitly[Monad[F]].functor
    bindFunctor[({type λ[α] = LazyEitherT[A, F, α]})#λ]
  }

  implicit def LazyLeftEitherTBindFunctor[F[_] : Monad, B]: BindFunctor[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, B]})#λ] = {
    implicit val b = implicitly[Monad[F]].bind
    implicit val f = implicitly[Monad[F]].functor
    bindFunctor[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, B]})#λ]
  }

  implicit val LazyOptionBindFunctor: BindFunctor[LazyOption] =
    bindFunctor[LazyOption]

  implicit def LazyEitherBindFunctor[A]: BindFunctor[({type λ[α] = LazyEither[A, α]})#λ] =
    bindFunctor[({type λ[α] = LazyEither[A, α]})#λ]

  implicit def LazyLeftEitherBindFunctor[B]: BindFunctor[({type λ[α] = LazyEither.LazyLeftProjection[α, B]})#λ] =
    bindFunctor[({type λ[α] = LazyEither.LazyLeftProjection[α, B]})#λ]

}

trait BindFunctorsLow {
  implicit def TraversableBindFunctor[CC[X] <: collection.TraversableLike[X, CC[X]] with Traversable[X] : CanBuildAnySelf]: BindFunctor[CC] = new BindFunctor[CC] {
    val functor = implicitly[Functor[CC]]
    val bind = implicitly[Bind[CC]]
  }
}