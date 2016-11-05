package scalaz

/**
  * A Double-ended queue, based on the Bankers Double Ended Queue as
  * described by C. Okasaki in "Purely Functional Data Structures"
  *
  * A queue that allows items to be put onto either the front (cons)
  * or the back (snoc) of the queue in constant time, and constant
  * time access to the element at the very front or the very back of
  * the queue.  Dequeueing an element from either end is constant time
  * when amortized over a number of dequeues.
  *
  * This queue maintains an invariant that whenever there are at least
  * two elements in the queue, neither the front list nor back list
  * are empty.  In order to maintain this invariant, a dequeue from
  * either side which would leave that side empty constructs the
  * resulting queue by taking elements from the opposite side
  */
sealed abstract class Dequeue[A] {
  import Maybe.Just
  import Dequeue.reverseNEL

  def isEmpty: Boolean

  def frontMaybe: Maybe[A]
  def backMaybe: Maybe[A]

  /**
    * dequeue from the front of the queue
    */
  def uncons: Maybe[(A, Dequeue[A])] = this match {
    case EmptyDequeue() => Maybe.empty
    case SingletonDequeue(a) => Just((a, EmptyDequeue()))
    case FullDequeue(OneAnd(f, INil()), 1, OneAnd(single,INil()), 1) => Just((f, SingletonDequeue(single)))
    case FullDequeue(OneAnd(f, INil()), 1, OneAnd(x, ICons(xx, xs)), bs) => {
      val xsr = reverseNEL(OneAnd(xx, xs))
      Just((f, FullDequeue(xsr, bs-1, OneAnd(x, IList.empty), 1)))
    }
    case FullDequeue(OneAnd(f, ICons(ff, fs)), s, back, bs) => Just(f, FullDequeue(OneAnd(ff, fs), s-1, back, bs))
  }

  /**
    * dequeue from the back of the queue
    */
  def unsnoc: Maybe[(A, Dequeue[A])] = this match {
    case EmptyDequeue() => Maybe.empty
    case SingletonDequeue(a) => Just((a, EmptyDequeue()))
    case FullDequeue(OneAnd(single, INil()), 1, OneAnd(b, INil()), 1) => Just((b, SingletonDequeue(single)))
    case FullDequeue(OneAnd(x, ICons(xx,xs)), fs, OneAnd(b, INil()), 1) => {
      val xsr = reverseNEL(OneAnd(xx, xs))
      Just((b, FullDequeue(OneAnd(x, IList.empty), 1, xsr, fs-1)))
    }

    case FullDequeue(front, fs, OneAnd(b, ICons(bb,bs)), s) => Just((b, FullDequeue(front, fs, OneAnd(bb,bs), s-1)))
  }


  /**
    * enqueue to the front of the queue
    */
  def cons(a: A): Dequeue[A] = this match {
    case EmptyDequeue() => SingletonDequeue(a)
    case SingletonDequeue(single) => FullDequeue(OneAnd(a, IList.empty), 1, OneAnd(single, IList.empty), 1 )
    case FullDequeue(front, fs, back, bs) => FullDequeue(OneAnd(a, ICons(front.head, front.tail)), fs+1, back, bs)
  }

  /**
    * enqueue on to the back of the queue
    */
  def snoc(a: A): Dequeue[A] = this match {
    case EmptyDequeue() => SingletonDequeue(a)
    case SingletonDequeue(single) => FullDequeue(OneAnd(single, INil[A]()), 1, OneAnd(a, INil[A]()), 1 )
    case FullDequeue(front, fs, back, bs) => FullDequeue(front, fs, OneAnd(a, ICons(back.head, back.tail)), bs+1)
  }

  /**
    * alias for cons
    */
  def +:(a: A): Dequeue[A] = cons(a)

  /**
    * alias for snoc
    */
  def :+(a: A): Dequeue[A] = snoc(a)

  /**
    * convert this queue to a stream of elements from front to back
    */
  def toStream: Stream[A] = std.stream.unfoldm(this)(_.uncons)

  /**
    * convert this queue to a stream of elements from back to front
    */
  def toBackStream: Stream[A] = std.stream.unfoldm(this)(_.unsnoc)

  /**
    * convert this queue to a list of elements from front to back
    */
  def toIList: IList[A] = this match {
    case EmptyDequeue() => IList.empty
    case SingletonDequeue(a) => ICons(a, IList.empty)
    case FullDequeue(front, fs, back, bs) => front.head +: (front.tail ++ (back.tail reverse_::: ICons(back.head, IList.empty)))
  }

  /**
    * convert this queue to a list of elements from back to front
    */
  def toBackIList: IList[A] = this match {
    case EmptyDequeue() => IList.empty
    case SingletonDequeue(a) => ICons(a, IList.empty)
    case FullDequeue(front, fs, back, bs) => back.head +: (back.tail ++ (front.tail reverse_::: ICons(front.head, IList.empty)))
  }

  /**
    * Append another Dequeue to this dequeue
    */
  def ++(other: Dequeue[A]): Dequeue[A] = this match {
    case EmptyDequeue() => other
    case SingletonDequeue(a) => a +: other
    case FullDequeue(f,fs,b,bs) => other match {
      case EmptyDequeue() => this
      case SingletonDequeue(a) => this :+ a
      case FullDequeue(of,ofs,ob,obs) =>
        FullDequeue(OneAnd(f.head, (f.tail ++
                                      ((b.head +: b.tail) reverse_:::
                                      ICons(of.head, of.tail)))),
                                    fs + bs + ofs,
                                    ob,
                                    obs)
    }
  }

  def foldLeft[B](b: B)(f: (B,A) => B): B = this match {
    case EmptyDequeue() => b
    case SingletonDequeue(a) => f(b, a)
    case FullDequeue(front,_,back,_) => {
      val frontb = front.tail.foldLeft(f(b,front.head))(f)
      val backb = back.tail.foldRight(frontb)((a, b) => f(b,a))
      f(backb,back.head)
    }
  }

  def foldRight[B](b: B)(f: (A,B) => B): B = this match {
    case EmptyDequeue() => b
    case SingletonDequeue(a) => f(a, b)
    case FullDequeue(front,_,back,_) => {
      val backb = back.tail.foldLeft(f(back.head, b))((b,a) => f(a,b))
      val frontb = front.tail.foldRight(backb)(f)
      f(front.head, frontb)
    }
  }


  def map[B](f: A => B): Dequeue[B] = {
    this match {
      case EmptyDequeue() => EmptyDequeue()
      case SingletonDequeue(a) => SingletonDequeue(f(a))
      case FullDequeue(front, fs, back, bs) => {
        val F = Functor[NonEmptyIList]
        FullDequeue(F.map(front)(f), fs, F.map(back)(f), bs)
      }
    }
  }

  def size: Int = this match {
    case EmptyDequeue() => 0
    case SingletonDequeue(_) => 1
    case FullDequeue(_, fs, _, bs) => fs + bs
  }

  def reverse: Dequeue[A] = this match {
    case FullDequeue(front, fs, back, bs) => FullDequeue(back, bs, front, fs)
    case x => x
  }
}

object Dequeue extends DequeueInstances {
  def apply[A](as: A*) = as.foldLeft[Dequeue[A]](empty)((q,a) ⇒ q :+ a)

  def fromFoldable[F[_],A](fa: F[A])(implicit F: Foldable[F]): Dequeue[A] =
    F.foldLeft[A,Dequeue[A]](fa,empty)((q,a) ⇒ q :+ a)

  def empty[A]: Dequeue[A] = EmptyDequeue()

  private def reverseNEL[A](fa: NonEmptyIList[A]): NonEmptyIList[A] = {
    @annotation.tailrec
    def loop(xs: IList[A], acc: IList[A]): NonEmptyIList[A] =
      (xs: @unchecked) match {
        case ICons(h, INil()) =>
          OneAnd(h, acc)
        case ICons(h, t) =>
          loop(t, h :: acc)
      }
    loop(fa.head :: fa.tail, IList.empty)
  }
}

/**
  * special case of the queue when it contains just a single element
  * which can be accessed from either side of the queue
  */
private[scalaz] final case class SingletonDequeue[A](single: A) extends Dequeue[A] {
  override def isEmpty = false
  override def frontMaybe = Maybe.Just(single)
  override def backMaybe = Maybe.Just(single)
}

/**
  * a queue which has at least two elements, it is guaranteed that the
  * front list and back lists cannot be empty
  */
private[scalaz] final case class FullDequeue[A](front: NonEmptyIList[A], fsize: Int, back: NonEmptyIList[A], backSize: Int) extends Dequeue[A]  {
  override def isEmpty = false
  override def frontMaybe = Maybe.just(front.head)
  override def backMaybe = Maybe.just(back.head)
}
/**
  * a queue which has no elements
  */
private[scalaz] final case object EmptyDequeue extends Dequeue[Nothing] {
  override val isEmpty = true
  override val frontMaybe = Maybe.empty
  override val backMaybe = Maybe.empty

  def apply[A]() = this.asInstanceOf[Dequeue[A]]
  def unapply[A](q: Dequeue[A]) = q.isEmpty
}

sealed abstract class DequeueInstances {
  implicit def equalDequeue[A](implicit A0: Equal[A]): Equal[Dequeue[A]] =
    new DequeueEqual[A] {
      val A = A0
    }

  implicit def dequeueMonoid[A]: Monoid[Dequeue[A]] = new Monoid[Dequeue[A]] {
    def zero: Dequeue[A] = Dequeue.empty
    def append(a: Dequeue[A], b: => Dequeue[A]): Dequeue[A] = a ++ b
  }

  implicit val dequeueInstances: Foldable[Dequeue] with IsEmpty[Dequeue] with Functor[Dequeue] = new Foldable[Dequeue] with IsEmpty[Dequeue] with Functor[Dequeue] {
    override def foldRight[A,B](fa: Dequeue[A], b: => B)(f: (A, => B) =>B): B = fa.foldRight(b)((a,b) => f(a,b))
    override def foldLeft[A,B](fa: Dequeue[A], b: B)(f: (B,A)=>B): B = fa.foldLeft(b)(f)
    override def foldMap[A,B](fa: Dequeue[A])(f: A => B)(implicit F: Monoid[B]): B = fa.foldLeft(F.zero)((b,a) => F.append(b, f(a)))
    override def empty[A]: Dequeue[A] = Dequeue.empty
    override def plus[A](a: Dequeue[A], b: => Dequeue[A]): Dequeue[A] = a ++ b
    override def isEmpty[A](fa: Dequeue[A]) = fa.isEmpty
    override def length[A](fa: Dequeue[A]) = fa.size
    override def map[A,B](fa: Dequeue[A])(f: A => B): Dequeue[B] = fa map f
  }
}

private[scalaz] trait DequeueEqual[A] extends Equal[Dequeue[A]] {
  implicit def A: Equal[A]
  import std.stream._

  final override def equal(a: Dequeue[A], b: Dequeue[A]): Boolean =
    Equal[Stream[A]].equal(a.toStream, b.toStream)
}

