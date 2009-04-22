package scalaz

trait Each[-E[_]] {
  def each[A](e: E[A], f: A => Unit): Unit
}

object Each {
  implicit val IdentityEach = new Each[Identity] {
    def each[A](e: Identity[A], f: A => Unit) = f(e.value)
  }

  implicit val ContinuationEach = new Each[PartialApply1Of2[Continuation, Unit]#Apply] {
    def each[A](e: Continuation[Unit, A], f: A => Unit) = e(f)
  }

  implicit def NonEmptyListEach[A] = new Each[NonEmptyList] {
    def each[A](e: NonEmptyList[A], f: A => Unit) = e.list foreach f 
  }

  implicit val StateEach = new Each[PartialApply1Of2[State, Unit]#Apply] {
    def each[A](e: State[Unit, A], f: A => Unit) = f(e(())._2)
  }

  implicit val ZipStreamEach = new Each[ZipStream] {
    def each[A](e: ZipStream[A], f: A => Unit) = e.value foreach f
  }

  implicit val Tuple1Each = new Each[Tuple1] {
    def each[A](e: Tuple1[A], f: A => Unit) = f(e._1)
  }

  implicit def Tuple2Each[R] = new Each[PartialApply1Of2[Tuple2, R]#Apply] {
    def each[A](e: (R, A), f: A => Unit) = f(e._2)
  }

  implicit def Tuple3Each[R, S] = new Each[PartialApply2Of3[Tuple3, R, S]#Apply] {
    def each[A](e: (R, S, A), f: A => Unit) = f(e._3)
  }

  implicit def Tuple4Each[R, S, T] = new Each[PartialApply3Of4[Tuple4, R, S, T]#Apply] {
    def each[A](e: (R, S, T, A), f: A => Unit) = f(e._4)
  }

  implicit def Tuple5Each[R, S, T, U] = new Each[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] {
    def each[A](e: (R, S, T, U, A), f: A => Unit) = f(e._5)
  }

  implicit def Tuple6Each[R, S, T, U, V] = new Each[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] {
    def each[A](e: (R, S, T, U, V, A), f: A => Unit) = f(e._6)
  }

  implicit def Tuple7Each[R, S, T, U, V, W] = new Each[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] {
    def each[A](e: (R, S, T, U, V, W, A), f: A => Unit) = f(e._7)
  }

  implicit val Function0Each = new Each[Function0] {
    def each[A](e: Function0[A], f: A => Unit) = f(e.apply)
  }

  implicit val OptionEach = new Each[Option] {
    def each[A](e: Option[A], f: A => Unit) = e foreach f
  }

  implicit val ArrayEach = new Each[Array] {
    def each[A](e: Array[A], f: A => Unit) = e foreach f
  }

  implicit def EitherLeftEach[X] = new Each[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def each[A](e: Either.LeftProjection[A, X], f: A => Unit) = e foreach f
  }

  implicit def EitherRightEach[X] = new Each[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def each[A](e: Either.RightProjection[X, A], f: A => Unit) = e foreach f
  }

  implicit val IterableEach = new Each[Iterable] {
      def each[A](e: Iterable[A], f: A => Unit) = e foreach f
  }

  implicit val JavaIterableEach = new Each[java.lang.Iterable] {
      def each[A](e: java.lang.Iterable[A], f: A => Unit) = {
        val i = e.iterator

        while(i.hasNext) {
          f(i.next)
        }
      }
  }
}
