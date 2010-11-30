package scalaz

trait Index[-I[_]] {
  def index[A](a: I[A], i: Int): Option[A]
}

object Index {
  import Scalaz._
  
  implicit def IdentityIndex: Index[Identity] = new Index[Identity] {
    def index[A](a: Identity[A], i: Int) = if(i == 0) Some(a.value) else None
  }

  implicit def NonEmptyListIndex[A]: Index[NonEmptyList] = new Index[NonEmptyList] {
    def index[A](a: NonEmptyList[A], i: Int) = if(i == 0) Some(a.head) else IterableIndex.index(a.tail, i - 1)
  }

  implicit def ZipStreamIndex: Index[ZipStream] = new Index[ZipStream] {
    def index[A](a: ZipStream[A], i: Int) = IterableIndex.index(a.value, i)
  }

  implicit def Tuple1Index: Index[Tuple1] = new Index[Tuple1] {
    def index[A](a: Tuple1[A], i: Int) = if(i == 0) Some(a._1) else None
  }

  implicit def Function0Index: Index[Function0] = new Index[Function0] {
    def index[A](a: () => A, i: Int) = if(i == 0) Some(a.apply) else None
  }

  implicit def OptionIndex: Index[Option] = new Index[Option] {
    def index[A](a: Option[A], i: Int) = a filter (_ => i == 0)
  }

  implicit def ArraySeqIndex: Index[ArraySeq] = new Index[ArraySeq] {
    def index[A](a: ArraySeq[A], i: Int) = if(i >= 0 && i < a.length) Some(a(i)) else None
  }

  implicit def ArrayIndex: Index[Array] = new Index[Array] {
    def index[A](a: Array[A], i: Int) = if(i >= 0 && i < a.length) Some(a(i)) else None
  }

  implicit def EitherLeftIndex[X]: Index[({type λ[α]=Either.LeftProjection[α, X]})#λ] = new Index[({type λ[α]=Either.LeftProjection[α, X]})#λ] {
    def index[A](a: Either.LeftProjection[A, X], i: Int) = a.toOption filter (_ => i == 0)
  }

  implicit def EitherRightIndex[X]: Index[({type λ[α]=Either.RightProjection[X, α]})#λ] = new Index[({type λ[α]=Either.RightProjection[X, α]})#λ] {
    def index[A](a: Either.RightProjection[X, A], i: Int) = a.toOption filter (_ => i == 0)
  }

  implicit def ValidationIndex[X]: Index[({type λ[α]=Validation[X, α]})#λ] = new Index[({type λ[α]=Validation[X, α]})#λ] {
    def index[A](a: Validation[X, A], i: Int) = a.either.right.toOption filter (_ => i == 0)
  }

  implicit def ValidationFailureIndex[X]: Index[({type λ[α]=FailProjection[α, X]})#λ] = new Index[({type λ[α]=FailProjection[α, X]})#λ] {
    def index[A](a: FailProjection[A, X], i: Int) = a.validation.either.left.toOption filter (_ => i == 0)
  }

  implicit def IterableIndex: Index[Iterable] = new Index[Iterable] {
    def index[A](a: Iterable[A], i: Int) = if(i < 0) None else {
      var n = 0
      var k: Option[A] = None
      val it = a.iterator
      while(it.hasNext && k.isEmpty) {
        val z = it.next
        if(n == i) k = Some(z)
        n = n + 1
      }

      k
    }
  }

  implicit def JavaIterableIndex: Index[java.lang.Iterable] = new Index[java.lang.Iterable] {
    def index[A](a: java.lang.Iterable[A], i: Int) = if(i < 0) None else {
      var n = 0
      var k: Option[A] = None
      val it = a.iterator
      while(it.hasNext && k.isEmpty) {
        val z = it.next
        if(n == i) k = Some(z)
        n = n + 1
      }

      k
    }
  }

  import java.util.concurrent.Callable

  implicit def CallableIndex: Index[Callable] = new Index[Callable] {
    def index[A](a: Callable[A], i: Int) = if(i == 0) Some(a.call) else None
  }
}
