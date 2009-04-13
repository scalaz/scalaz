package scalaz

trait Pure[+P[_]] {
  implicit def pure[A](a: A): P[A]
}

object Pure {
  implicit val IdentityPure: Pure[Identity] = new Pure[Identity] {
    def pure[A](a: A) = Identity.id(a)
  }

  implicit def ContinuationPure[R] = new Pure[PartialApply1Of2[Continuation, R]#Apply] {
    def pure[A](a: A) = Continuation.continuation[R, A](_(a))
  }

  implicit val NonEmptyListPure = new Pure[NonEmptyList] {
    def pure[A](a: A) = NonEmptyList.nel(a)
  }

  implicit def StatePure[S] = new Pure[PartialApply1Of2[State, S]#Apply] {
    def pure[A](a: A) = State.value[S](a)
  }

  implicit val Tuple1Pure = new Pure[Tuple1] {
    def pure[A](a: A) = Tuple1(a)
  }

  implicit val Function0Pure = new Pure[Function0] {
    def pure[A](a: A) = new Function0[A] {
      def apply = a
    }
  }

  implicit def Function1Pure[R] = new Pure[PartialApply1Of2[Function1, R]#Apply] {
    def pure[A](a: A) = (_: R) => a
  }

  implicit def Function2Pure[R, S] = new Pure[PartialApply2Of3[Function2, R, S]#Apply] {
    def pure[A](a: A) = (_: R, _: S) => a
  }

  implicit def Function3Pure[R, S, T] = new Pure[PartialApply3Of4[Function3, R, S, T]#Apply] {
    def pure[A](a: A) = (_: R, _: S , _: T ) => a
  }

  implicit def Function4Pure[R, S, T, U] = new Pure[PartialApply4Of5[Function4, R, S, T, U]#Apply] {
    def pure[A](a: A) = (_: R, _: S , _: T , _: U) => a
  }

  implicit def Function5Pure[R, S, T, U, V] = new Pure[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] {
    def pure[A](a: A) = (_: R, _: S , _: T , _: U, _: V) => a
  }

  implicit def Function6Pure[R, S, T, U, V, W] = new Pure[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] {
    def pure[A](a: A) = (_: R, _: S , _: T , _: U, _: V, _: W) => a
  }

  implicit val ListPure = new Pure[List] {
    def pure[A](a: A) = List(a)
  }

  implicit val StreamPure = new Pure[Stream] {
    def pure[A](a: A) = Stream(a)
  }

  implicit val OptionPure = new Pure[Option] {
    def pure[A](a: A) = Some(a)
  }

  implicit val ArrayPure = new Pure[Array] {
    def pure[A](a: A) = Array.make(1, a)
  }

  implicit def EitherLeftPure[X] = new Pure[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def pure[A](a: A) = Left(a).left
  }

  implicit def EitherRightPure[X] = new Pure[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def pure[A](a: A) = Right(a).right
  }

  implicit val RandomAccessSeqPure = new Pure[RandomAccessSeq] {
    def pure[A](a: A) = RandomAccessSeq(a)
  }
}
