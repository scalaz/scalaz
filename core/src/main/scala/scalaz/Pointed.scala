package scalaz

trait Pointed[F[_]] {
  def point[A](a: => A): F[A]

  def **[G[_] : Pointed]: Pointed[({type λ[α] = (F[α], G[α])})#λ] =
    new Pointed[({type λ[α] = (F[α], G[α])})#λ] {
      def point[A](a: => A) =
        (Pointed.this.point(a), implicitly[Pointed[G]].point(a))
    }

  def deriving[G[_]](implicit n: ^**^[G, F]): Pointed[G] =
    new Pointed[G] {
      def point[A](a: => A) =
        n.pack(Pointed.this.point(a))
    }

}

object Pointed extends Pointeds

trait Pointeds extends PointedsLow {

  import java.util.concurrent.Callable

  implicit val OptionPointed: Pointed[Option] = new Pointed[Option] {
    def point[A](a: => A) = Some(a)
  }

  implicit val ListPointed: Pointed[List] = new Pointed[List] {
    def point[A](a: => A) = List(a)
  }

  implicit val StreamPointed: Pointed[Stream] = new Pointed[Stream] {
    def point[A](a: => A) = Stream(a)
  }

  implicit def CallablePointed: Pointed[Callable] = new Pointed[Callable] {
    def point[A](a: => A) = new Callable[A] {
      def call = a
    }
  }

  implicit def EitherLeftPointed[X]: Pointed[({type λ[α] = Either.LeftProjection[α, X]})#λ] = new Pointed[({type λ[α] = Either.LeftProjection[α, X]})#λ] {
    def point[A](a: => A) = Left(a).left
  }

  implicit def EitherRightPointed[X]: Pointed[({type λ[α] = Either.RightProjection[X, α]})#λ] = new Pointed[({type λ[α] = Either.RightProjection[X, α]})#λ] {
    def point[A](a: => A) = Right(a).right
  }

  implicit def EitherPointed[X]: Pointed[({type λ[α] = Either[X, α]})#λ] = new Pointed[({type λ[α] = Either[X, α]})#λ] {
    def point[A](a: => A) = Right(a)
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryPointed[X: Zero]: Pointed[({type λ[α] = Entry[X, α]})#λ] = new Pointed[({type λ[α] = Entry[X, α]})#λ] {
    def point[A](a: => A) =
      new SimpleImmutableEntry(implicitly[Zero[X]].zero, a)
  }

  implicit def Tuple1Pointed: Pointed[Tuple1] = new Pointed[Tuple1] {
    def point[A](a: => A) = Tuple1(a)
  }

  implicit def Tuple2Pointed[R: Zero]: Pointed[({type λ[α] = (R, α)})#λ] = new Pointed[({type λ[α] = (R, α)})#λ] {
    def point[A](a: => A) = (implicitly[Zero[R]].zero, a)
  }

  implicit def Tuple3Pointed[R: Zero, S: Zero]: Pointed[({type λ[α] = (R, S, α)})#λ] = new Pointed[({type λ[α] = (R, S, α)})#λ] {
    def point[A](a: => A) = (implicitly[Zero[R]].zero, implicitly[Zero[S]].zero, a)
  }

  implicit def Tuple4Pointed[R: Zero, S: Zero, T: Zero]: Pointed[({type λ[α] = (R, S, T, α)})#λ] = new Pointed[({type λ[α] = (R, S, T, α)})#λ] {
    def point[A](a: => A) = (implicitly[Zero[R]].zero, implicitly[Zero[S]].zero, implicitly[Zero[T]].zero, a)
  }

  implicit def Tuple5Pointed[R: Zero, S: Zero, T: Zero, U: Zero]: Pointed[({type λ[α] = (R, S, T, U, α)})#λ] = new Pointed[({type λ[α] = (R, S, T, U, α)})#λ] {
    def point[A](a: => A) = (implicitly[Zero[R]].zero, implicitly[Zero[S]].zero, implicitly[Zero[T]].zero, implicitly[Zero[U]].zero, a)
  }

  implicit def Tuple6Pointed[R: Zero, S: Zero, T: Zero, U: Zero, V: Zero]: Pointed[({type λ[α] = (R, S, T, U, V, α)})#λ] = new Pointed[({type λ[α] = (R, S, T, U, V, α)})#λ] {
    def point[A](a: => A) = (implicitly[Zero[R]].zero, implicitly[Zero[S]].zero, implicitly[Zero[T]].zero, implicitly[Zero[U]].zero, implicitly[Zero[V]].zero, a)
  }

  implicit def Tuple7Pointed[R: Zero, S: Zero, T: Zero, U: Zero, V: Zero, W: Zero]: Pointed[({type λ[α] = (R, S, T, U, V, W, α)})#λ] = new Pointed[({type λ[α] = (R, S, T, U, V, W, α)})#λ] {
    def point[A](a: => A) = (implicitly[Zero[R]].zero, implicitly[Zero[S]].zero, implicitly[Zero[T]].zero, implicitly[Zero[U]].zero, implicitly[Zero[V]].zero, implicitly[Zero[W]].zero, a)
  }

  implicit def Function0Pure: Pointed[Function0] = new Pointed[Function0] {
    def point[A](a: => A) = new Function0[A] {
      def apply = a
    }
  }

  implicit def Function1Pointed[R]: Pointed[({type λ[α] = (R) => α})#λ] = new Pointed[({type λ[α] = (R) => α})#λ] {
    def point[A](a: => A) = (_: R) => a
  }

  implicit def Function2Pointed[R, S]: Pointed[({type λ[α] = (R, S) => α})#λ] = new Pointed[({type λ[α] = (R, S) => α})#λ] {
    def point[A](a: => A) = (_: R, _: S) => a
  }

  implicit def Function3Pointed[R, S, T]: Pointed[({type λ[α] = (R, S, T) => α})#λ] = new Pointed[({type λ[α] = (R, S, T) => α})#λ] {
    def point[A](a: => A) = (_: R, _: S, _: T) => a
  }

  implicit def Function4Pointed[R, S, T, U]: Pointed[({type λ[α] = (R, S, T, U) => α})#λ] = new Pointed[({type λ[α] = (R, S, T, U) => α})#λ] {
    def point[A](a: => A) = (_: R, _: S, _: T, _: U) => a
  }

  implicit def Function5Pointed[R, S, T, U, V]: Pointed[({type λ[α] = (R, S, T, U, V) => α})#λ] = new Pointed[({type λ[α] = (R, S, T, U, V) => α})#λ] {
    def point[A](a: => A) = (_: R, _: S, _: T, _: U, _: V) => a
  }

  implicit def Function6Pointed[R, S, T, U, V, W]: Pointed[({type λ[α] = (R, S, T, U, V, W) => α})#λ] = new Pointed[({type λ[α] = (R, S, T, U, V, W) => α})#λ] {
    def point[A](a: => A) = (_: R, _: S, _: T, _: U, _: V, _: W) => a
  }

  implicit val IdentityPointed: Pointed[Identity] = new Pointed[Identity] {
    def point[A](a: => A) = Identity.id(a)
  }

  implicit def CoStatePointed[A: Zero, F[_] : Pointed]: Pointed[({type λ[α] = CoStateT[A, F, α]})#λ] = new Pointed[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def point[Z](z: => Z) =
      CoStateT.coStateT[A, F, Z]((implicitly[Pointed[F]].point(_ => z), implicitly[Zero[A]].zero))
  }

  implicit def KleisliPointed[F[_], R](implicit p: Pointed[F]): Pointed[({type λ[α] = Kleisli[R, F, α]})#λ] =
    new Pointed[({type λ[α] = Kleisli[R, F, α]})#λ] {
      def point[A](a: => A) =
        Kleisli.kleisli(_ => p.point(a))
    }

  implicit val NonEmptyListPointed: Pointed[NonEmptyList] = new Pointed[NonEmptyList] {
    def point[A](a: => A) =
      NonEmptyList.nels(a)
  }

  implicit def StateTPointed[A, F[_] : Pointed]: Pointed[({type λ[α] = StateT[A, F, α]})#λ] =
    new Pointed[({type λ[α] = StateT[A, F, α]})#λ] {
      def point[A](a: => A) =
        StateT.stateT(s => implicitly[Pointed[F]].point((a, s)))
    }

  implicit def StepListTPointed[F[_] : Pointed]
  : Pointed[({type λ[X] = StepListT[F, X]})#λ] = new Pointed[({type λ[X] = StepListT[F, X]})#λ] {
    def point[A](a: => A) =
      a :: StepListT.stepListT[F, A]
  }

  implicit def StepStreamTPointed[F[_] : Pointed]
  : Pointed[({type λ[X] = StepStreamT[F, X]})#λ] = new Pointed[({type λ[X] = StepStreamT[F, X]})#λ] {
    def point[A](a: => A) =
      a :: StepStreamT.stepStreamT[F, A]
  }

  implicit val TreePointed: Pointed[Tree] = new Pointed[Tree] {
    def point[A](a: => A) = Tree.leaf(a)
  }

  implicit def FailProjectionPointed[X]: Pointed[({type λ[α] = FailProjection[α, X]})#λ] =
    new Pointed[({type λ[α] = FailProjection[α, X]})#λ] {
      def point[A](a: => A) =
        Failure(a).fail
    }

  implicit def ValidationPointed[X]: Pointed[({type λ[α] = Validation[X, α]})#λ] = new Pointed[({type λ[α] = Validation[X, α]})#λ] {
    def point[A](a: => A) =
      Success(a)
  }

  implicit def WriterTPointed[A: Zero, F[_] : Pointed]: Pointed[({type λ[α] = WriterT[A, F, α]})#λ] =
    new Pointed[({type λ[α] = WriterT[A, F, α]})#λ] {
      def point[X](a: => X) =
        WriterT.writerT(implicitly[Pointed[F]].point((implicitly[Zero[A]].zero, a)))
    }

}

trait PointedsLow {

  import collection.TraversableLike

  implicit def TraversablePointed[CC[X] <: TraversableLike[X, CC[X]] : CanBuildAnySelf]: Pointed[CC] = new Pointed[CC] {
    def point[A](a: => A) = {
      val builder = implicitly[CanBuildAnySelf[CC]].apply[Nothing, A]
      builder += a
      builder.result
    }
  }

}