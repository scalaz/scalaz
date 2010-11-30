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
    def len[A](a: () => A) = 1
  }

  implicit def OptionLength: Length[Option] = new Length[Option] {
    def len[A](a: Option[A]) = a map (_ => 1) getOrElse 0
  }

  implicit def EitherLeftLength[X]: Length[({type λ[α]=Either.LeftProjection[α, X]})#λ] = new Length[({type λ[α]=Either.LeftProjection[α, X]})#λ] {
    def len[A](a: Either.LeftProjection[A, X]) = a.e match {
      case Right(_) => 0
      case Left(_) => 1
    }
  }

  implicit def EitherRightLength[X]: Length[({type λ[α]=Either.RightProjection[X, α]})#λ] = new Length[({type λ[α]=Either.RightProjection[X, α]})#λ] {
    def len[A](a: Either.RightProjection[X, A]) = a.e match {
      case Right(_) => 1
      case Left(_) => 0
    }
  }

  implicit def ValidationLength[X]: Length[({type λ[α]=Validation[X, α]})#λ] = new Length[({type λ[α]=Validation[X, α]})#λ] {
    def len[A](a: Validation[X, A]) = a match {
      case Success(_) => 1
      case Failure(_) => 0
    }
  }

  implicit def ValidationFailureLength[X]: Length[({type λ[α]=FailProjection[α, X]})#λ] = new Length[({type λ[α]=FailProjection[α, X]})#λ] {
    def len[A](a: FailProjection[A, X]) = a.validation match {
      case Success(_) => 0
      case Failure(_) => 1
    }
  }

  implicit def ArraySeqLength: Length[ArraySeq] = new Length[ArraySeq] {
    def len[A](a: ArraySeq[A]) = a.length
  }

  implicit def ArrayLength: Length[Array] = new Length[Array] {
    def len[A](a: Array[A]) = a.length
  }

  implicit def ImmutableArrayLength: Length[ImmutableArray] = new Length[ImmutableArray] {
    def len[A](a: ImmutableArray[A]) = a.length
  }

  import FingerTree.ftip2ft
  implicit def RopeLength: Length[Rope] = new Length[Rope] {
    def len[A](a: Rope[A]) = a.value.measure
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

  import java.util.concurrent.Callable

  implicit def CallableLength: Length[Callable] = new Length[Callable] {
    def len[A](a: Callable[A]) = 1
  }
}
