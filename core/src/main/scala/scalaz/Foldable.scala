package scalaz
import Scalaz._
/**
 * Data structures that can be folded.
 * Minimal complete definition: 'foldMap' or 'foldRight'.
 **/
trait Foldable[F[_]] {

  /** Combine the elements of a structure using a monoid. **/
  def fold[M: Monoid](t: F[M]): M = foldMap[M, M](t, x => x)
  
  /** Map each element of the structure to a monoid, and combine the results. **/
  def foldMap[A, M: Monoid](t: F[A], f: A => M): M =
    foldRight[A, M](t, mzero, (x, y) => f(x) |+| y)

  /** Right-associative fold of a structure. **/
  def foldRight[A, B](t: F[A], z: => B, f: (A, => B) => B): B = 
    foldMap(t, (a: A) => ((f.curried(a)(_: B)): Endo[B])) apply z

  /** Left-associative fold of a structure. **/
  def foldLeft[A, B](t: F[A], z: B, f: (B, A) => B): B =
    foldMap(t, (a: A) => (f.flip.curried(a): Endo[B]) σ).value(z)
  
  /** A variant of 'foldr' that has no base case,
   *  and thus is undefined for empty structures. **/
  def foldr1[A, B](t: F[A], f: (A, => A) => A): Option[A] = {
    def mf(x: A, o: => Option[A]) = o.map(f(x, _)) orElse Some(x)
    foldRight(t, None, mf)
  }

  /** A variant of 'foldl' that has no base case,
   *  and thus may only be applied to non-empty structures. **/
  def foldl1[A, B](t: F[A], f: (A, A) => A): Option[A] = {
    def mf(o: Option[A], x: A) = o.map(f(x, _)) orElse Some(x)
    foldLeft(t, None, mf)
  }
}

object Foldable extends FoldableCollections {
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
      while (!s.isEmpty) { r = op(s.pop, r) }
      r
    }    
  }

  implicit def StateFoldable: Foldable[PartialApply1Of2[State, Unit]#Apply] = new Foldable[PartialApply1Of2[State, Unit]#Apply] {
    override def foldLeft[A, B](t: State[Unit, A], b: B, f: (B, A) => B) = f(b, t(())._2)
    override def foldRight[A, B](t: State[Unit, A], b: => B, f: (A, => B) => B) = f(t(())._2, b)
  }

  implicit def Tuple1Foldable: Foldable[Tuple1] = new Foldable[Tuple1] {
    override def foldLeft[A, B](t: Tuple1[A], b: B, f: (B, A) => B) = f(b, t._1)
    override def foldRight[A, B](t: Tuple1[A], b: => B, f: (A, => B) => B) = f(t._1, b)
  }

  implicit def Function0Foldable: Foldable[Function0] = new Foldable[Function0] {
    override def foldLeft[A, B](t: Function0[A], b: B, f: (B, A) => B) = f(b, t.apply)
    override def foldRight[A, B](t: Function0[A], b: => B, f: (A, => B) => B) = f(t.apply, b)
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

  implicit def EitherLeftFoldable[X]: Foldable[PartialApply1Of2[Either.LeftProjection, X]#Flip] = new Foldable[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    override def foldLeft[A, B](e: Either.LeftProjection[A, X], b: B, f: (B, A) => B) = OptionFoldable.foldLeft(e.toOption, b, f)
    override def foldRight[A, B](e: Either.LeftProjection[A, X], b: => B, f: (A, => B) => B) = OptionFoldable.foldRight(e.toOption, b, f)
  }

  implicit def EitherRightFoldable[X]: Foldable[PartialApply1Of2[Either.RightProjection, X]#Apply] = new Foldable[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    override def foldLeft[A, B](e: Either.RightProjection[X, A], b: B, f: (B, A) => B) = OptionFoldable.foldLeft(e.toOption, b, f)
    override def foldRight[A, B](e: Either.RightProjection[X, A], b: => B, f: (A, => B) => B) = OptionFoldable.foldRight(e.toOption, b, f)
  }

  implicit def ValidationFoldable[X] = new Foldable[PartialApply1Of2[Validation, X]#Apply] {
    override def foldLeft[A, B](e: Validation[X, A], b: B, f: (B, A) => B) = e match {
      case Success(a) => f(b, a)
      case Failure(_) => b
    }
    override def foldRight[A, B](e: Validation[X, A], b: => B, f: (A, => B) => B) = e match {
      case Success(a) => f(a, b)
      case Failure(_) => b
    }
  }

  implicit def ValidationFailureFoldable[X] = new Foldable[PartialApply1Of2[FailProjection, X]#Flip] {
    override def foldLeft[A, B](e: FailProjection[A, X], b: B, f: (B, A) => B) = e.validation match {
      case Success(_) => b
      case Failure(e) => f(b, e)
    }
    override def foldRight[A, B](e: FailProjection[A, X], b: => B, f: (A, => B) => B) = e.validation match {
      case Success(_) => b
      case Failure(e) => f(e, b)
    }
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
