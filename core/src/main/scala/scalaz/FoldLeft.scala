package scalaz

trait FoldLeft[-F[_]] {
  def foldLeft[B, A](t: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  import Scalaz._

  implicit def IdentityFoldLeft: FoldLeft[Identity] = new FoldLeft[Identity] {
    def foldLeft[B, A](t: Identity[A], b: B, f: (B, A) => B) = f(b, t.value)
  }

  implicit def NonEmptyListFoldLeft: FoldLeft[NonEmptyList] = new FoldLeft[NonEmptyList] {
    def foldLeft[B, A](t: NonEmptyList[A], b: B, f: (B, A) => B) = t.list.foldLeft(b)(f)
  }

  implicit def StateFoldLeft: FoldLeft[PartialApply1Of2[State, Unit]#Apply] = new FoldLeft[PartialApply1Of2[State, Unit]#Apply] {
    def foldLeft[B, A](t: State[Unit, A], b: B, f: (B, A) => B) = f(b, t(())._2)
  }

  implicit def Tuple1FoldLeft: FoldLeft[Tuple1] = new FoldLeft[Tuple1] {
    def foldLeft[B, A](t: Tuple1[A], b: B, f: (B, A) => B) = f(b, t._1)
  }

  implicit def Function0FoldLeft: FoldLeft[Function0] = new FoldLeft[Function0] {
    def foldLeft[B, A](t: Function0[A], b: B, f: (B, A) => B) = f(b, t.apply)
  }

  implicit def OptionFoldLeft: FoldLeft[Option] = new FoldLeft[Option] {
    def foldLeft[B, A](t: Option[A], b: B, f: (B, A) => B) = t match {
      case Some(a) => f(b, a)
      case None => b
    }
  }

  implicit def TreeFoldLeft: FoldLeft[Tree] = new FoldLeft[Tree] {
    def foldLeft[B, A](t: Tree[A], b: B, f: (B, A) => B): B =
      t.foldMap((a: A) => (f.flip.curried(a): Endo[B]) σ).value(b)
  }

  implicit def ZipperFoldLeft: FoldLeft[Zipper] = new FoldLeft[Zipper] {
    def foldLeft[B, A](t: Zipper[A], b: B, f: (B, A) => B): B =
      t.lefts.foldRight((t.focus #:: t.rights).foldLeft(b)(f))(f.flip)
  }

  implicit def ZipStreamFoldLeft: FoldLeft[ZipStream] = new FoldLeft[ZipStream] {
    def foldLeft[B, A](t: ZipStream[A], b: B, f: (B, A) => B): B = TraversableOnceFoldLeft.foldLeft(t.value, b, f)
  }

  implicit def EitherLeftFoldLeft[X]: FoldLeft[PartialApply1Of2[Either.LeftProjection, X]#Flip] = new FoldLeft[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def foldLeft[B, A](e: Either.LeftProjection[A, X], b: B, f: (B, A) => B) = OptionFoldLeft.foldLeft(e.toOption, b, f)
  }

  implicit def EitherRightFoldLeft[X]: FoldLeft[PartialApply1Of2[Either.RightProjection, X]#Apply] = new FoldLeft[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def foldLeft[B, A](e: Either.RightProjection[X, A], b: B, f: (B, A) => B) = OptionFoldLeft.foldLeft(e.toOption, b, f)
  }

  implicit def ValidationFoldLeft[X] = new FoldLeft[PartialApply1Of2[Validation, X]#Apply] {
    def foldLeft[B, A](e: Validation[X, A], b: B, f: (B, A) => B) = e match {
      case Success(a) => f(b, a)
      case Failure(_) => b
    }
  }

  implicit def ValidationFailureFoldLeft[X] = new FoldLeft[PartialApply1Of2[FailProjection, X]#Flip] {
    def foldLeft[B, A](e: FailProjection[A, X], b: B, f: (B, A) => B) = e.validation match {
      case Success(_) => b
      case Failure(e) => f(b, e)
    }
  }

  implicit def TraversableOnceFoldLeft[C[X] <: TraversableOnce[X]]: FoldLeft[C] = new FoldLeft[C] {
    def foldLeft[B, A](t: C[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }

  implicit def JavaIterableFoldLeft: FoldLeft[java.lang.Iterable] = new FoldLeft[java.lang.Iterable] {
    def foldLeft[B, A](t: java.lang.Iterable[A], b: B, f: (B, A) => B) = {
      var x = b
      val i = t.iterator

      while (i.hasNext) {
        val n = i.next
        x = f(x, n)
      }

      x
    }
  }

  import java.util.concurrent.Callable

  implicit def CallableFoldLeft: FoldLeft[Callable] = new FoldLeft[Callable] {
    def foldLeft[B, A](t: Callable[A], b: B, f: (B, A) => B) = f(b, t.call)
  }
}
