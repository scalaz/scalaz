package scalaz

sealed trait Monad[M[_]] {
  implicit val pure: Pure[M]
  implicit val bind: Bind[M]

  implicit val functor = new Functor[M] {
    def fmap[A, B](fa: M[A], f: A => B) = bind.bind(fa, (a: A) => pure.pure(f(a)))
  }

  implicit val pointed = Pointed.pointed[M]

  implicit val apply = new Apply[M] {
    def apply[A, B](f: M[A => B], a: M[A]): M[B] = bind.bind(f, (k: A => B) => functor.fmap(a, k(_: A)))
  }
}

object Monad {
  def monad[M[_]](implicit b: Bind[M], p: Pure[M]) = new Monad[M] {
    val pure = p
    val bind = b
  }                 /*

  implicit val IdentityMonad = monad[Identity]

  implicit def ContinuationMonad[R] = monad[PartialApply1Of2[Continuation, R]#Apply]

  implicit def OptionMonad = monad[Option]
                      */
  /*
  implicit val IdentityMonad: Monad[Identity] = new Monad[Identity] {
    def fmap[A, B](r: Identity[A], f: A => Identity[B]) = Identity.id(f(r.value))
  }

  implicit def ContinuationMonad[R] = new Monad[PartialApply1Of2[Continuation, R]#Apply] {
    def fmap[A, B](r: Continuation[R, A], f: A => B) = Continuation.continuation[R, B](k => r(k compose f))
  }

  implicit val NonEmptyListMonad = new Monad[NonEmptyList] {
    def fmap[A, B](r: NonEmptyList[A], f: A => NonEmptyList[B]) = r map f
  }

  implicit def StateMonad[S] = new Monad[PartialApply1Of2[State, S]#Apply] {
    def fmap[A, B](r: State[S, A], f: A => State[S, B]) = r map f
  }

  implicit val Tuple1Monad = new Monad[Tuple1] {
    def fmap[A, B](r: Tuple1[A], f: A => Tuple1[B]) = Tuple1(f(r._1))
  }

  implicit def Tuple2Monad[R] = new Monad[PartialApply1Of2[Tuple2, R]#Apply] {
    def fmap[A, B](r: (R, A), f: A => (R, B)) = (r._1, f(r._2))
  }

  implicit def Tuple3Monad[R, S] = new Monad[PartialApply2Of3[Tuple3, R, S]#Apply] {
    def fmap[A, B](r: (R, S, A), f: A => (R, S, B)) = (r._1, r._2, f(r._3))
  }

  implicit def Tuple4Monad[R, S, T] = new Monad[PartialApply3Of4[Tuple4, R, S, T]#Apply] {
    def fmap[A, B](r: (R, S, T, A), f: A => (R, S, T, B)) = (r._1, r._2, r._3, f(r._4))
  }

  implicit def Tuple5Monad[R, S, T, U] = new Monad[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] {
    def fmap[A, B](r: (R, S, T, U, A), f: A => (R, S, T, U, B)) = (r._1, r._2, r._3, r._4, f(r._5))
  }

  implicit def Tuple6Monad[R, S, T, U, V] = new Monad[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] {
    def fmap[A, B](r: (R, S, T, U, V, A), f: A => (R, S, T, U, V, B)) = (r._1, r._2, r._3, r._4, r._5, f(r._6))
  }

  implicit def Tuple7Monad[R, S, T, U, V, W] = new Monad[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] {
    def fmap[A, B](r: (R, S, T, U, V, W, A), f: A => (R, S, T, U, V, W, B)) = (r._1, r._2, r._3, r._4, r._5, r._6, f(r._7))
  }

  implicit val Function0Monad = new Monad[Function0] {
    def fmap[A, B](r: Function0[A], f: A => Function0[B]) = new Function0[B] {
      def apply = f(r.apply)
    }
  }

  implicit def Function1Monad[R] = new Monad[PartialApply1Of2[Function1, R]#Apply] {
    def fmap[A, B](r: R => A, f: A => R => B) = r andThen f
  }

  implicit def Function2Monad[R, S] = new Monad[PartialApply2Of3[Function2, R, S]#Apply] {
    def fmap[A, B](r: (R, S) => A, f: A => (R, S) => B) = (t1: R, t2: S) => f(r(t1, t2))
  }

  implicit def Function3Monad[R, S, T] = new Monad[PartialApply3Of4[Function3, R, S, T]#Apply] {
    def fmap[A, B](r: (R, S, T) => A, f: A => (R, S, T) => B) = (t1: R, t2: S, t3: T) => f(r(t1, t2, t3))
  }

  implicit def Function4Monad[R, S, T, U] = new Monad[PartialApply4Of5[Function4, R, S, T, U]#Apply] {
    def fmap[A, B](r: (R, S, T, U) => A, f: A => (R, S, T, U) => B) = (t1: R, t2: S, t3: T, t4: U) => f(r(t1, t2, t3, t4))
  }

  implicit def Function5Monad[R, S, T, U, V] = new Monad[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] {
    def fmap[A, B](r: (R, S, T, U, V) => A, f: A => (R, S, T, U, V) => B) = (t1: R, t2: S, t3: T, t4: U, t5: V) => f(r(t1, t2, t3, t4, t5))
  }

  implicit def Function6Monad[R, S, T, U, V, W] = new Monad[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] {
    def fmap[A, B](r: (R, S, T, U, V, W) => A, f: A => (R, S, T, U, V, W) => B) = (t1: R, t2: S, t3: T, t4: U, t5: V, t6: W) => f(r(t1, t2, t3, t4, t5, t6))
  }

  implicit val ListMonad = new Monad[List] {
    def fmap[A, B](r: List[A], f: A => List[B]) = r map f
  }

  implicit val StreamMonad = new Monad[Stream] {
    def fmap[A, B](r: Stream[A], f: A => Stream[B]) = r map f
  }

  implicit val OptionMonad = new Monad[Option] {
    def fmap[A, B](r: Option[A], f: A => Option[B]) = r map f
  }

  implicit val ArrayMonad = new Monad[Array] {
    def fmap[A, B](r: Array[A], f: A => Array[B]) = r map f
  }

  implicit def EitherLeftMonad[X] = new Monad[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def fmap[A, B](r: Either.LeftProjection[A, X], f: A => Either.LeftProjection[B, X]) = r.map(f).left
  }

  implicit def EitherRightMonad[X] = new Monad[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def fmap[A, B](r: Either.RightProjection[X, A], f: A => Either.RightProjection[X, B]) = r.map(f).right
  }

  implicit val RandomAccessSeqMonad = new Monad[RandomAccessSeq] {
    def fmap[A, B](r: RandomAccessSeq[A], f: A => RandomAccessSeq[B]) = r.projection map f projection
  }
                                                       */

}
