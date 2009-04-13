package scalaz

trait FoldLeft[-F[_]] {
  def foldLeft[B, A](t: F[A], b: B, f: (B, A) => B): B
}

object FoldLeft {
  implicit val IdentityFoldLeft = new FoldLeft[Identity] {
    def foldLeft[B, A](t: Identity[A], b: B, f: (B, A) => B) = f(b, t.value)
  }

  implicit def NonEmptyListEach[A] = new FoldLeft[NonEmptyList] {
    def foldLeft[B, A](t: NonEmptyList[A], b: B, f: (B, A) => B) = t.list.foldLeft(b)(f)
  }

  implicit val StateFoldLeft = new FoldLeft[PartialApply1Of2[State, Unit]#Apply] {
    def foldLeft[B, A](t: State[Unit, A], b: B, f: (B, A) => B) = f(b, t(())._2)
  }

  implicit def Tuple1Each[A] = new FoldLeft[Tuple1] {
    def foldLeft[B, A](t: Tuple1[A], b: B, f: (B, A) => B) = f(b, t._1)
  }

  implicit def Function0Each[A] = new FoldLeft[Function0] {
    def foldLeft[B, A](t: Function0[A], b: B, f: (B, A) => B) = f(b, t.apply)
  }

  implicit def ArrayEach[A] = new FoldLeft[Array] {
    def foldLeft[B, A](t: Array[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }

  implicit def IterableEach[A] = new FoldLeft[Iterable] {
    def foldLeft[B, A](t: Iterable[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }

  implicit def JavaIterableEach[A] = new FoldLeft[java.lang.Iterable] {
    def foldLeft[B, A](t: java.lang.Iterable[A], b: B, f: (B, A) => B) = {
      var x = b
      val i = t.iterator

      while(i.hasNext) {
        val n = i.next
        x = f(x, n)
      }

      x
    }
  }
}
