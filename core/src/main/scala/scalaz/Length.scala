package scalaz

trait Length[-L[_]] {
  def len[A](a: L[A]): Int
}

object Length {
  import Scalaz._

  implicit def IdentityLength: Length[Identity] = new Length[Identity] {
    def len[A](a: Identity[A]) = 1
  }

  implicit def NonEmptyListLength[A]: Length[NonEmptyList] = new Length[NonEmptyList] {
    def len[A](a: NonEmptyList[A]) = a.list.length
  }

  implicit def ZipStreamLength: Length[ZipStream] = new Length[ZipStream] {
    def len[A](a: ZipStream[A]) = a.value.length
  }

  implicit def Tuple1Length: Length[Tuple1] = new Length[Tuple1] {
    def len[A](a: Tuple1[A]) = 1
  }

  implicit def Function0Length: Length[Function0] = new Length[Function0] {
    def len[A](a: Function0[A]) = 1
  }

  implicit def OptionLength: Length[Option] = new Length[Option] {
    def len[A](a: Option[A]) = a map (_ => 1) getOrElse 0
  }

  implicit def EitherLeftLength[X]: Length[PartialApply1Of2[Either.LeftProjection, X]#Flip] = new Length[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def len[A](a: Either.LeftProjection[A, X]) = a.e match {
      case Right(_) => 0
      case Left(_) => 1
    }
  }

  implicit def EitherRightLength[X]: Length[PartialApply1Of2[Either.RightProjection, X]#Apply] = new Length[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def len[A](a: Either.RightProjection[X, A]) = a.e match {
      case Right(_) => 1
      case Left(_) => 0
    }
  }

  implicit def ValidationLength[X]: Length[PartialApply1Of2[Validation, X]#Apply] = new Length[PartialApply1Of2[Validation, X]#Apply] {
    def len[A](a: Validation[X, A]) = a match {
      case Success(_) => 1
      case Failure(_) => 0
    }
  }

  implicit def ValidationFailureLength[X]: Length[PartialApply1Of2[FailProjection, X]#Flip] = new Length[PartialApply1Of2[FailProjection, X]#Flip] {
    def len[A](a: FailProjection[A, X]) = a.validation match {
      case Success(_) => 0
      case Failure(_) => 1
    }
  }

  implicit def GenericArrayLength: Length[GArray] = new Length[GArray] {
    def len[A](a: GArray[A]) = a.length
  }

  implicit def IterableLength: Length[Iterable] = new Length[Iterable] {
    def len[A](a: Iterable[A]) = {
      var n = 0
      val i = a.iterator
      while(i.hasNext) {
        n = n + 1
        i.next
      }

      n
    }
  }

  implicit def JavaIterableLength: Length[java.lang.Iterable] = new Length[java.lang.Iterable] {
    def len[A](a: java.lang.Iterable[A]) = {
      var n = 0
      val i = a.iterator
      while(i.hasNext) {
        n = n + 1
        i.next
      }

      n
    }
  }
}
