package scalaz

trait Functor[F[_]] {
  def fmap[A, B](r: F[A], f: A => B): F[B]
}

object Functor {
  implicit val IdentityFunctor: Functor[Identity] = new Functor[Identity] {
    def fmap[A, B](r: Identity[A], f: A => B) = Identity.id(f(r.value))
  }

  implicit def ContinuationFunctor[R] = new Functor[PartialApply1Of2[Continuation, R]#Apply] {
    def fmap[A, B](r: Continuation[R, A], f: A => B) = Continuation.continuation[R, B](k => r(k compose f))
  }

  implicit val NonEmptyListFunctor = new Functor[NonEmptyList] {
    def fmap[A, B](r: NonEmptyList[A], f: A => B) = r map f
  }

  implicit def StateFunctor[S] = new Functor[PartialApply1Of2[State, S]#Apply] {
    def fmap[A, B](r: State[S, A], f: A => B) = r map f
  }

  implicit val Tuple1Functor = new Functor[Tuple1] {
    def fmap[A, B](r: Tuple1[A], f: A => B) = Tuple1(f(r._1))
  }

  implicit def Tuple2Functor[R] = new Functor[PartialApply1Of2[Tuple2, R]#Apply] {
    def fmap[A, B](r: (R, A), f: A => B) = (r._1, f(r._2))
  }

  implicit val Function0Functor = new Functor[Function0] {
    def fmap[A, B](r: Function0[A], f: A => B) = new Function0[B] {
      def apply = f(r.apply)
    }
  }

  implicit def Function1Functor[R] = new Functor[PartialApply1Of2[Function1, R]#Apply] {
    def fmap[A, B](r: R => A, f: A => B) = r andThen f
  }

  implicit val ListFunctor = new Functor[List] {
    def fmap[A, B](r: List[A], f: A => B) = r map f
  }

  implicit val StreamFunctor = new Functor[Stream] {
    def fmap[A, B](r: Stream[A], f: A => B) = r map f
  }

  implicit val OptionFunctor = new Functor[Option] {
    def fmap[A, B](r: Option[A], f: A => B) = r map f
  }

  implicit val ArrayFunctor = new Functor[Array] {
    def fmap[A, B](r: Array[A], f: A => B) = r map f
  }

  implicit def EitherLeftFunctor[X] = new Functor[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def fmap[A, B](r: Either.LeftProjection[A, X], f: A => B) = r.map(f).left
  }

  implicit def EitherRightFunctor[X] = new Functor[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def fmap[A, B](r: Either.RightProjection[X, A], f: A => B) = r.map(f).right
  }
}
