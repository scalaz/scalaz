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

  implicit def Tuple1Each[A] = new Each[Tuple1] {
    def each[A](e: Tuple1[A], f: A => Unit) = f(e._1) 
  }

  implicit def Function0Each[A] = new Each[Function0] {
    def each[A](e: Function0[A], f: A => Unit) = f(e.apply)
  }

  implicit def OptionEach[A] = new Each[Option] {
    def each[A](e: Option[A], f: A => Unit) = e foreach f
  }

  implicit def ArrayEach[A] = new Each[Array] {
    def each[A](e: Array[A], f: A => Unit) = e foreach f
  }

  implicit def EitherLeftEach[X] = new Each[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def each[A](e: Either.LeftProjection[A, X], f: A => Unit) = e foreach f
  }

  implicit def EitherRightEach[X] = new Each[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def each[A](e: Either.RightProjection[X, A], f: A => Unit) = e foreach f
  }

  implicit def IterableEach[A] = new Each[Iterable] {
      def each[A](e: Iterable[A], f: A => Unit) = e foreach f
  }

  implicit def JavaIterableEach[A] = new Each[java.lang.Iterable] {
      def each[A](e: java.lang.Iterable[A], f: A => Unit) = {
        val i = e.iterator

        while(i.hasNext) {
          f(i.next)
        }
      }
  }
}
