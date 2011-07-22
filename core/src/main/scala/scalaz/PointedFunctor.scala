package scalaz

trait PointedFunctor[F[_]] {
  val functor: Functor[F]
  val pointed: Pointed[F]

  import PointedFunctor._

  def compose[G[_]](gtr: PointedFunctor[G]): PointedFunctor[({type λ[α] = F[G[α]]})#λ] =
    pointedFunctor[({type λ[α] = F[G[α]]})#λ](
      new Functor[({type λ[α] = F[G[α]]})#λ] {
        def fmap[A, B](f: A => B) =
          functor.fmap(gtr.fmap(f))
      }
      , new Pointed[({type λ[α] = F[G[α]]})#λ] {
        def point[A](a: => A) =
          pointed.point(gtr.point(a))
      }
    )

  def **[G[_] : PointedFunctor]: PointedFunctor[({type λ[α] = (F[α], G[α])})#λ] = {
    implicit val f = functor ** implicitly[PointedFunctor[G]].functor
    implicit val p = pointed ** implicitly[PointedFunctor[G]].pointed
    pointedFunctor[({type λ[α] = (F[α], G[α])})#λ]
  }

  def point[A](a: => A): F[A] =
    pointed.point(a)

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def deriving[G[_]](implicit n: ^**^[G, F]): PointedFunctor[G] = {
    implicit val f: Functor[G] = functor.deriving[G]
    implicit val p: Pointed[G] = pointed.deriving[G]
    pointedFunctor[G]
  }

}

object PointedFunctor extends PointedFunctors

trait PointedFunctors {
  def pointedFunctor[F[_]](implicit f: Functor[F], p: Pointed[F]): PointedFunctor[F] = new PointedFunctor[F] {
    val functor = f
    val pointed = p
  }

  implicit val OptionPointedFunctor: PointedFunctor[Option] =
    pointedFunctor

  implicit val ListPointedFunctor: PointedFunctor[List] =
    pointedFunctor

  implicit val StreamPointedFunctor: PointedFunctor[Stream] =
    pointedFunctor

  implicit def EitherLeftPointedFunctor[X] =
    pointedFunctor[({type λ[α] = Either.LeftProjection[α, X]})#λ]

  implicit def EitherRightPointedFunctor[X] =
    pointedFunctor[({type λ[α] = Either.RightProjection[X, α]})#λ]

  implicit def EitherPointedFunctor[X] =
    pointedFunctor[({type λ[α] = Either[X, α]})#λ]

  import java.util.Map.Entry

  implicit def MapEntryPointedFunctor[X: Monoid] = {
    implicit val z = implicitly[Monoid[X]].zero
    implicit val s = implicitly[Monoid[X]].semigroup
    pointedFunctor[({type λ[α] = Entry[X, α]})#λ]
  }

  implicit def Tuple1PointedFunctor =
    pointedFunctor[Tuple1]

  implicit def Tuple2PointedFunctor[R: Zero] =
    pointedFunctor[({type λ[α] = (R, α)})#λ]

  implicit def Tuple3PointedFunctor[R: Zero, S: Zero] =
    pointedFunctor[({type λ[α] = (R, S, α)})#λ]

  implicit def Tuple4PointedFunctor[R: Zero, S: Zero, T: Zero] =
    pointedFunctor[({type λ[α] = (R, S, T, α)})#λ]

  implicit def Tuple5PointedFunctor[R: Zero, S: Zero, T: Zero, U: Zero] =
    pointedFunctor[({type λ[α] = (R, S, T, U, α)})#λ]

  implicit def Tuple6PointedFunctor[R: Zero, S: Zero, T: Zero, U: Zero, V: Zero] =
    pointedFunctor[({type λ[α] = (R, S, T, U, V, α)})#λ]

  implicit def Tuple7PointedFunctor[R: Zero, S: Zero, T: Zero, U: Zero, V: Zero, W: Zero] =
    pointedFunctor[({type λ[α] = (R, S, T, U, V, W, α)})#λ]

  implicit def Function0PointedFunctor: PointedFunctor[Function0] =
    pointedFunctor[Function0]

  implicit def Function1PointedFunctor[R]: PointedFunctor[({type λ[α] = (R) => α})#λ] =
    pointedFunctor[({type λ[α] = (R) => α})#λ]

  implicit def Function2PointedFunctor[R, S]: PointedFunctor[({type λ[α] = (R, S) => α})#λ] =
    pointedFunctor[({type λ[α] = (R, S) => α})#λ]

  implicit def Function3PointedFunctor[R, S, T]: PointedFunctor[({type λ[α] = (R, S, T) => α})#λ] =
    pointedFunctor[({type λ[α] = (R, S, T) => α})#λ]

  implicit def Function4PointedFunctor[R, S, T, U]: PointedFunctor[({type λ[α] = (R, S, T, U) => α})#λ] =
    pointedFunctor[({type λ[α] = (R, S, T, U) => α})#λ]

  implicit def Function5PointedFunctor[R, S, T, U, V]: PointedFunctor[({type λ[α] = (R, S, T, U, V) => α})#λ] =
    pointedFunctor[({type λ[α] = (R, S, T, U, V) => α})#λ]

  implicit def Function6PointedFunctor[R, S, T, U, V, W]: PointedFunctor[({type λ[α] = (R, S, T, U, V, W) => α})#λ] =
    pointedFunctor[({type λ[α] = (R, S, T, U, V, W) => α})#λ]

  implicit val IdentityPointedFunctor: PointedFunctor[Identity] =
    pointedFunctor[Identity]

  implicit def CoKleisliPointedFunctor[F[_], R]: PointedFunctor[({type λ[α] = CoKleisli[R, F, α]})#λ] =
    pointedFunctor[({type λ[α] = CoKleisli[R, F, α]})#λ]

  implicit def ConstPointedFunctor[A: Zero]: PointedFunctor[({type λ[α] = Const[A, α]})#λ] =
    pointedFunctor[({type λ[α] = Const[A, α]})#λ]

  implicit def CoStatePointedFunctor[A: Zero, F[_] : PointedFunctor]: PointedFunctor[({type λ[α] = CoStateT[A, F, α]})#λ] = new PointedFunctor[({type λ[α] = CoStateT[A, F, α]})#λ] {
    implicit val ftr = implicitly[PointedFunctor[F]].functor
    implicit val pt = implicitly[PointedFunctor[F]].pointed
    val functor = implicitly[Functor[({type λ[α] = CoStateT[A, F, α]})#λ]]
    val pointed = implicitly[Pointed[({type λ[α] = CoStateT[A, F, α]})#λ]]
  }

  implicit def KleisliPointedFunctor[F[_], R](implicit pt: PointedFunctor[F]): PointedFunctor[({type λ[α] = Kleisli[R, F, α]})#λ] = {
    implicit val p = pt.pointed
    implicit val f = pt.functor
    pointedFunctor[({type λ[α] = Kleisli[R, F, α]})#λ]
  }

  implicit val NonEmptyListPointedFunctor: PointedFunctor[NonEmptyList] =
    pointedFunctor

  implicit def ReaderWriterStateTPointedFunctor[R, W: Zero, S, F[_] : PointedFunctor]: PointedFunctor[({type λ[α] = ReaderWriterStateT[R, W, S, F, α]})#λ] = new PointedFunctor[({type λ[α] = ReaderWriterStateT[R, W, S, F, α]})#λ] {
    implicit val ftr = implicitly[PointedFunctor[F]].functor
    implicit val pt = implicitly[PointedFunctor[F]].pointed
    val functor = implicitly[Functor[({type λ[α] = ReaderWriterStateT[R, W, S, F, α]})#λ]]
    val pointed = implicitly[Pointed[({type λ[α] = ReaderWriterStateT[R, W, S, F, α]})#λ]]
  }

  implicit def StateTPointedFunctor[A, F[_] : PointedFunctor]: PointedFunctor[({type λ[α] = StateT[A, F, α]})#λ] = new PointedFunctor[({type λ[α] = StateT[A, F, α]})#λ] {
    implicit val ftr = implicitly[PointedFunctor[F]].functor
    implicit val pt = implicitly[PointedFunctor[F]].pointed
    val functor = implicitly[Functor[({type λ[α] = StateT[A, F, α]})#λ]]
    val pointed = implicitly[Pointed[({type λ[α] = StateT[A, F, α]})#λ]]
  }

  implicit def StepListTPointedFunctor[F[_]](implicit pf: PointedFunctor[F]): PointedFunctor[({type λ[X] = StepListT[F, X]})#λ] = {
    implicit val p = pf.pointed
    implicit val ftr = pf.functor
    pointedFunctor[({type λ[X] = StepListT[F, X]})#λ]
  }

  implicit def StepStreamTPointedFunctor[F[_]](implicit pf: PointedFunctor[F]): PointedFunctor[({type λ[X] = StepStreamT[F, X]})#λ] = {
    implicit val p = pf.pointed
    implicit val ftr = pf.functor
    pointedFunctor[({type λ[X] = StepStreamT[F, X]})#λ]
  }

  implicit val TreePointedFunctor: PointedFunctor[Tree] =
    pointedFunctor[Tree]

  implicit def FailProjectionPointedFunctor[X]: PointedFunctor[({type λ[α] = FailProjection[α, X]})#λ] =
    pointedFunctor[({type λ[α] = FailProjection[α, X]})#λ]

  implicit def ValidationPointedFunctor[X]: PointedFunctor[({type λ[α] = Validation[X, α]})#λ] =
    pointedFunctor[({type λ[α] = Validation[X, α]})#λ]

  implicit def WriterTPointedFunctor[A: Zero, F[_] : PointedFunctor]: PointedFunctor[({type λ[α] = WriterT[A, F, α]})#λ] = new PointedFunctor[({type λ[α] = WriterT[A, F, α]})#λ] {
    implicit val ftr = implicitly[PointedFunctor[F]].functor
    implicit val pt = implicitly[PointedFunctor[F]].pointed
    val functor = implicitly[Functor[({type λ[α] = WriterT[A, F, α]})#λ]]
    val pointed = implicitly[Pointed[({type λ[α] = WriterT[A, F, α]})#λ]]
  }

  implicit def OptionTPointedFunctor[F[_] : PointedFunctor]: PointedFunctor[({type λ[α] = OptionT[F, α]})#λ] = {
    implicit val p = implicitly[PointedFunctor[F]].pointed
    implicit val f = implicitly[PointedFunctor[F]].functor
    pointedFunctor[({type λ[α] = OptionT[F, α]})#λ]
  }

  implicit def LazyOptionTPointedFunctor[F[_] : PointedFunctor]: PointedFunctor[({type λ[α] = LazyOptionT[F, α]})#λ] = {
    implicit val p = implicitly[PointedFunctor[F]].pointed
    implicit val f = implicitly[PointedFunctor[F]].functor
    pointedFunctor[({type λ[α] = LazyOptionT[F, α]})#λ]
  }

  implicit def EitherTPointedFunctor[F[_] : PointedFunctor, A]: PointedFunctor[({type λ[α] = EitherT[A, F, α]})#λ] = {
    implicit val p = implicitly[PointedFunctor[F]].pointed
    implicit val f = implicitly[PointedFunctor[F]].functor
    pointedFunctor[({type λ[α] = EitherT[A, F, α]})#λ]
  }

  implicit def LeftEitherTPointedFunctor[F[_] : PointedFunctor, B]: PointedFunctor[({type λ[α] = EitherT.LeftProjectionT[α, F, B]})#λ] = {
    implicit val p = implicitly[PointedFunctor[F]].pointed
    implicit val f = implicitly[PointedFunctor[F]].functor
    pointedFunctor[({type λ[α] = EitherT.LeftProjectionT[α, F, B]})#λ]
  }

  implicit def LazyEitherTPointedFunctor[F[_] : PointedFunctor, A]: PointedFunctor[({type λ[α] = LazyEitherT[A, F, α]})#λ] = {
    implicit val p = implicitly[PointedFunctor[F]].pointed
    implicit val f = implicitly[PointedFunctor[F]].functor
    pointedFunctor[({type λ[α] = LazyEitherT[A, F, α]})#λ]
  }

  implicit def LazyLeftEitherTPointedFunctor[F[_] : PointedFunctor, B]: PointedFunctor[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, B]})#λ] = {
    implicit val p = implicitly[PointedFunctor[F]].pointed
    implicit val f = implicitly[PointedFunctor[F]].functor
    pointedFunctor[({type λ[α] = LazyEitherT.LazyLeftProjectionT[α, F, B]})#λ]
  }

  implicit val LazyOptionPointedFunctor: PointedFunctor[LazyOption] =
    pointedFunctor[LazyOption]

  implicit def LazyEitherPointedFunctor[A]: PointedFunctor[({type λ[α] = LazyEither[A, α]})#λ] =
    pointedFunctor[({type λ[α] = LazyEither[A, α]})#λ]

  implicit def LazyLeftEitherPointedFunctor[B]: PointedFunctor[({type λ[α] = LazyEither.LazyLeftProjection[α, B]})#λ] =
    pointedFunctor[({type λ[α] = LazyEither.LazyLeftProjection[α, B]})#λ]

  import scala.util.control.TailCalls.TailRec
  implicit val TailRecPointedFunctor : PointedFunctor[TailRec] =
    pointedFunctor[TailRec]

  import scala.util.continuations.ControlContext
  implicit def ControlContextPointedFunctor[B] : PointedFunctor[({type T[A] = ControlContext[A,B,B]})#T] =
    pointedFunctor[({type T[A] = ControlContext[A,B,B]})#T]

}
