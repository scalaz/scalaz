package scalaz

import Scalaz._

/**
 * Data structures that can be folded.
 * Minimal complete definition: 'foldMap' or 'foldRight'.
 **/
trait Foldable[F[_]] {

  /**Combine the elements of a structure using a monoid. **/
  def fold[M: Monoid](t: F[M]): M = foldMap[M, M](t, x => x)

  /**Map each element of the structure to a monoid, and combine the results. **/
  def foldMap[A, M: Monoid](t: F[A], f: A => M): M =
    foldRight[A, M](t, mzero[M], (x, y) => f(x) |+| y)

  /**Right-associative fold of a structure. **/
  def foldRight[A, B](t: F[A], z: => B, f: (A, => B) => B): B =
    foldMap(t, (a: A) => (EndoTo(f.curried(a)(_: B)))) apply z

  /**Left-associative fold of a structure. **/
  def foldLeft[A, B](t: F[A], z: B, f: (B, A) => B): B =
    foldMap(t, (a: A) => (EndoTo(f.flip.curried(a))) σ).value(z)

  /**A variant of 'foldr' that has no base case,
   *  and thus is undefined for empty structures. **/
  def foldr1[A, B](t: F[A], f: (A, => A) => A): Option[A] = {
    def mf(x: A, o: => Option[A]) = o.map(f(x, _)) orElse Some(x)
    foldRight(t, None, mf)
  }

  /**A variant of 'foldl' that has no base case,
   *  and thus may only be applied to non-empty structures. **/
  def foldl1[A, B](t: F[A], f: (A, A) => A): Option[A] = {
    def mf(o: Option[A], x: A) = o.map(f(x, _)) orElse Some(x)
    foldLeft(t, None, mf)
  }
}

trait FoldableLow {
 implicit def TraversableFoldable[CC[X] <: Traversable[X]]: Foldable[CC] = new Foldable[CC] {
    override def foldRight[A, B](t: CC[A], b: => B, f: (A, => B) => B): B = t.foldRight(b)(f(_, _))
   
    override def foldLeft[A, B](t: CC[A], b: B, f: (B, A) => B): B = t.foldLeft(b)(f(_, _))
  }
}

object Foldable extends FoldableLow {
  import Scalaz._

  implicit def IdentityFoldable: Foldable[Identity] = new Foldable[Identity] {
    override def foldLeft[A, B](t: Identity[A], b: B, f: (B, A) => B) = f(b, t.value)

    override def foldRight[A, B](t: Identity[A], b: => B, f: (A, => B) => B) = f(t.value, b)
  }

  def IterableSubtypeFoldable[I[X] <: Iterable[X]]: Foldable[I] = new Foldable[I] {
    override def foldRight[A, B](t: I[A], b: => B, f: (A, => B) => B): B = t.foldRight(b)(f(_, _))
  }

  implicit def NonEmptyListFoldable: Foldable[NonEmptyList] = new Foldable[NonEmptyList] {
    override def foldLeft[A, B](t: NonEmptyList[A], b: B, f: (B, A) => B) = t.list.foldLeft(b)(f)

    override def foldRight[A, B](t: NonEmptyList[A], b: => B, f: (A, => B) => B) = ListFoldable.foldRight(t.list, b, f)
  }

  implicit val ListFoldable: Foldable[List] = new Foldable[List] {
    override def foldLeft[A, B](t: List[A], z: B, f: (B, A) => B): B =
      t.foldLeft(z)(f)

    override def foldRight[A, B](t: List[A], z: => B, op: (A, => B) => B): B = {
      import scala.collection.mutable.ArrayStack
      val s = new ArrayStack[A]
      t.foreach(a => s += a)
      var r = z
      while (!s.isEmpty) {r = op(s.pop, r)}
      r
    }
  }

  implicit def StreamFoldable[A]: Foldable[Stream] = new Foldable[Stream] {
    override def foldRight[A, B](t: Stream[A], b: => B, f: (A, => B) => B): B =
      if (t.isEmpty)
        b
      else
        f(t.head, foldRight(t.tail, b, f))

    override def foldLeft[A, B](t: Stream[A], b: B, f: (B, A) => B): B = t.foldLeft(b)(f(_, _))
  }

  implicit def StateFoldable: Foldable[({type λ[α]=State[Unit, α]})#λ] = new Foldable[({type λ[α]=State[Unit, α]})#λ] {
    override def foldLeft[A, B](t: State[Unit, A], b: B, f: (B, A) => B) = f(b, t(())._2)

    override def foldRight[A, B](t: State[Unit, A], b: => B, f: (A, => B) => B) = f(t(())._2, b)
  }

  implicit def Tuple1Foldable: Foldable[Tuple1] = new Foldable[Tuple1] {
    override def foldLeft[A, B](t: Tuple1[A], b: B, f: (B, A) => B) = f(b, t._1)

    override def foldRight[A, B](t: Tuple1[A], b: => B, f: (A, => B) => B) = f(t._1, b)
  }

  implicit def Function0Foldable: Foldable[Function0] = new Foldable[Function0] {
    override def foldLeft[A, B](t: () => A, b: B, f: (B, A) => B) = f(b, t.apply)

    override def foldRight[A, B](t: () => A, b: => B, f: (A, => B) => B) = f(t.apply, b)
  }

  implicit def OptionFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldLeft[A, B](t: Option[A], b: B, f: (B, A) => B) = t match {
      case Some(a) => f(b, a)
      case None => b
    }

    override def foldRight[A, B](t: Option[A], b: => B, f: (A, => B) => B) = t match {
      case Some(a) => f(a, b)
      case None => b
    }
  }

  implicit def TreeFoldable: Foldable[Tree] = new Foldable[Tree] {
    override def foldMap[A, M: Monoid](t: Tree[A], f: A => M) = t.foldMap(f)
  }

  implicit def ZipperFoldable: Foldable[Zipper] = new Foldable[Zipper] {
    override def foldLeft[A, B](t: Zipper[A], b: B, f: (B, A) => B): B =
      t.lefts.foldRight((t.focus #:: t.rights).foldLeft(b)(f))(f.flip)

    override def foldRight[A, B](t: Zipper[A], b: => B, f: (A, => B) => B): B =
      t.lefts.foldLeft(Stream.cons(t.focus, t.rights).foldRight(b)(f(_, _)))((f.flip)(_, _))
  }

  implicit def ZipStreamFoldable: Foldable[ZipStream] = new Foldable[ZipStream] {
    override def foldLeft[A, B](t: ZipStream[A], b: B, f: (B, A) => B): B = implicitly[Foldable[Stream]].foldLeft(t.value, b, f)

    override def foldRight[A, B](t: ZipStream[A], b: => B, f: (A, => B) => B): B = implicitly[Foldable[Stream]].foldRight(t.value, b, f)
  }

  implicit def EitherLeftFoldable[X]: Foldable[({type λ[α]=Either.LeftProjection[α, X]})#λ] = new Foldable[({type λ[α]=Either.LeftProjection[α, X]})#λ] {
    override def foldLeft[A, B](e: Either.LeftProjection[A, X], b: B, f: (B, A) => B) = OptionFoldable.foldLeft(e.toOption, b, f)

    override def foldRight[A, B](e: Either.LeftProjection[A, X], b: => B, f: (A, => B) => B) = OptionFoldable.foldRight(e.toOption, b, f)
  }

  implicit def EitherRightFoldable[X]: Foldable[({type λ[α]=Either.RightProjection[X, α]})#λ] = new Foldable[({type λ[α]=Either.RightProjection[X, α]})#λ] {
    override def foldLeft[A, B](e: Either.RightProjection[X, A], b: B, f: (B, A) => B) = OptionFoldable.foldLeft(e.toOption, b, f)

    override def foldRight[A, B](e: Either.RightProjection[X, A], b: => B, f: (A, => B) => B) = OptionFoldable.foldRight(e.toOption, b, f)
  }

  implicit def ValidationFoldable[X] = new Foldable[({type λ[α]=Validation[X, α]})#λ] {
    override def foldLeft[A, B](e: Validation[X, A], b: B, f: (B, A) => B) = e match {
      case Success(a) => f(b, a)
      case Failure(_) => b
    }

    override def foldRight[A, B](e: Validation[X, A], b: => B, f: (A, => B) => B) = e match {
      case Success(a) => f(a, b)
      case Failure(_) => b
    }
  }

  implicit def ValidationFailureFoldable[X] = new Foldable[({type λ[α]=FailProjection[α, X]})#λ] {
    override def foldLeft[A, B](e: FailProjection[A, X], b: B, f: (B, A) => B) = e.validation match {
      case Success(_) => b
      case Failure(e) => f(b, e)
    }

    override def foldRight[A, B](e: FailProjection[A, X], b: => B, f: (A, => B) => B) = e.validation match {
      case Success(_) => b
      case Failure(e) => f(e, b)
    }
  }

  implicit def FingerFoldable[V] = new Foldable[({type λ[α]=Finger[V, α]})#λ] {
    override def foldMap[A, M: Monoid](t: Finger[V, A], f: A => M): M = t foldMap f
  }

  implicit def NodeFoldable[V] = new Foldable[({type λ[α]=Node[V, α]})#λ] {
    override def foldMap[A, M: Monoid](t: Node[V, A], f: A => M): M = t foldMap f
  }

  implicit def FingerTreeFoldable[V]:Foldable[({type λ[α]=FingerTree[V, α]})#λ] =
    new Foldable[({type λ[α]=FingerTree[V, α]})#λ] {
    override def foldMap[A, M: Monoid](t: FingerTree[V, A], f: A => M): M = t foldMap f
    override def foldRight[A, B](t: FingerTree[V, A], b: => B, f: (A, => B) => B): B =
      t.fold(v => b,
             (v, a) => f(a, b), 
             (v, pr, m, sf) =>
               FingerFoldable[V].foldRight(pr,
                                           foldRight[Node[V, A], B](m,
                                                     FingerFoldable[V].foldRight(sf, b, f),
                                                     (x, y) => NodeFoldable[V].foldRight(x, y, f)),
                                           f))
    override def foldLeft[A, B](t: FingerTree[V, A], b: B, f: (B, A) => B): B =
      t.fold(v => b,
             (v, a) => f(b, a), 
             (v, pr, m, sf) =>
               FingerFoldable[V].foldLeft(pr,
                                           foldLeft[Node[V, A], B](m,
                                                     FingerFoldable[V].foldLeft(sf, b, f),
                                                     (x, y) => NodeFoldable[V].foldLeft(y, x, f)),
                                           f))
  }

  implicit def JavaIterableFoldable: Foldable[java.lang.Iterable] = new Foldable[java.lang.Iterable] {
    override def foldLeft[A, B](t: java.lang.Iterable[A], b: B, f: (B, A) => B) = {
      var x = b
      val i = t.iterator

      while (i.hasNext) {
        val n = i.next
        x = f(x, n)
      }

      x
    }

    override def foldRight[A, B](t: java.lang.Iterable[A], b: => B, f: (A, => B) => B) = {
      val i = new Iterable[A] {
        override def iterator = new Iterator[A] {
          val k = t.iterator

          def hasNext = k.hasNext

          def next = k.next
        }
      }
      IterableSubtypeFoldable[Iterable].foldRight[A, B](i, b, f)
    }
  }

  import java.util.concurrent.Callable

  implicit def CallableFoldable: Foldable[Callable] = new Foldable[Callable] {
    override def foldLeft[A, B](t: Callable[A], b: B, f: (B, A) => B) = f(b, t.call)

    override def foldRight[A, B](t: Callable[A], b: => B, f: (A, => B) => B) = f(t.call, b)
  }
}
