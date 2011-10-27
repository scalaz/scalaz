package scalaz
package std

import annotation.tailrec

trait Lists {
  implicit val listInstance = new MonadPlus[List] with Traverse[List] with Empty[List] with Each[List] with Index[List] with Length[List] {
    def each[A](fa: List[A])(f: (A) => Unit): Unit = fa foreach f
    def index[A](fa: List[A], i: Int): Option[A] = {
      var n = 0
      var k: Option[A] = None
      val it = fa.iterator
      while (it.hasNext && k.isEmpty) {
        val z = it.next()
        if (n == i) k = Some(z)
        n = n + 1
      }

      k
    }
    def length[A](fa: List[A]): Int = fa.length
    def pure[A](a: => A): List[A] = scala.List(a)
    def bind[A, B](fa: List[A])(f: A => List[B]) = fa flatMap f
    def empty[A] = scala.List()
    def plus[A](a: List[A], b: => List[A]) = a ++ b
    override def map[A, B](l: List[A])(f: A => B) = l map f

    def traverseImpl[F[_], A, B](l: List[A])(f: A => F[B])(implicit F: Applicative[F]): F[List[B]] = {
      val fbs: F[List[B]] = F.pure(scala.List[B]())

      l.reverse.foldLeft(fbs)((fbl, a) => F.map2(f(a), fbl)(_ :: _))
    }

    def foldR[A, B](fa: List[A], z: B)(f: (A) => (=> B) => B): B = fa.foldRight(z)((a, b) => f(a)(b))
  }

  implicit def listMonoid[A] = new Monoid[List[A]] {
    def append(f1: List[A], f2: => List[A]): List[A] = f1 ::: f2
    def zero: List[A] = Nil
  }

  //
  // Functions for Lists
  //

  def intersperse[A](as: List[A], a: A): List[A] = {
    @tailrec
    def intersperse0(accum: List[A], rest: List[A]): List[A] = rest match {
      case Nil      => accum
      case x :: Nil => x :: accum
      case h :: t   => intersperse0(a :: h :: accum, t)
    }
    intersperse0(Nil, as).reverse
  }

  def intercalate[A](as1: List[A], as2: List[A]): List[A] = {
    val asr = as2.reverse
    @tailrec
    def intercalate0(accum: List[A], rest: List[A]): List[A] = rest match {
      case Nil      => accum
      case x :: Nil => x :: accum
      case h :: t   => intercalate0(asr ::: h :: accum, t)
    }
    intercalate0(Nil, as1).reverse
  }

  def toNel[A](as: List[A]): Option[NonEmptyList[A]] = as match {
    case Nil    => None
    case h :: t => Some(NonEmptyList.nel(h, t))
  }

  def toZipper[A](as: List[A]): Option[Zipper[A]] =
    stream.toZipper(as.toStream)

  def zipperEnd[A](as: List[A]): Option[Zipper[A]] =
    stream.zipperEnd(as.toStream)

  def <^>[A, B: Monoid](as: List[A])(f: NonEmptyList[A] => B): B = as match {
    case Nil    => Monoid[B].zero
    case h :: t => f(NonEmptyList.nel(h, t))
  }

  def takeWhileM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[List[A]] = as match {
    case Nil    => Monad[M].pure(Nil)
    case h :: t => Monad[M].bind(p(h))(b =>
      if (b) Monad[M].map(takeWhileM(t)(p))((tt: List[A]) => h :: tt) else Monad[M].pure(Nil))
  }

  def takeUntilM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[List[A]] =
    takeWhileM(as)((a: A) => Monad[M].map(p(a))((b) => !b))

  def filterM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[List[A]] = as match {
    case Nil    => Monad[M].pure(Nil)
    case h :: t => {
      def g = filterM(t)(p)
      Monad[M].bind(p(h))(b => if (b) Monad[M].map(g)(tt => h :: tt) else g)
    }
  }

  def findM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[Option[A]] = as match {
    case Nil    => Monad[M].pure(None: Option[A])
    case h :: t => Monad[M].bind(p(h))(b =>
      if (b) Monad[M].pure(Some(h): Option[A]) else findM(t)(p))
  }

  def powerset[A](as: List[A]): List[List[A]] = filterM(as)(_ => scala.List(true, false))

  def partitionM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[(List[A], List[A])] = as match {
    case Nil    => Monad[M].pure(Nil: List[A], Nil: List[A])
    case h :: t =>
      Monad[M].bind(p(h))(b =>
        Monad[M].map(partitionM(t)(p)) {
          case (x, y) =>
            if (b) (h :: x, y) else (x, h :: y)
        }
      )
  }

  def spanM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[(List[A], List[A])] = as match {
    case Nil    => Monad[M].pure(Nil, Nil)
    case h :: t =>
      Monad[M].bind(p(h))(b =>
        if (b) Monad[M].map(spanM(t)(p))((k: (List[A], List[A])) => (h :: k._1, k._2))
        else Monad[M].pure(Nil, as))

  }

  def breakM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[(List[A], List[A])] =
    spanM(as)(a => Monad[M].map(p(a))((b: Boolean) => !b))

  def groupByM[A, M[_] : Monad](as: List[A])(p: (A, A) => M[Boolean]): M[List[List[A]]] = as match {
    case Nil    => Monad[M].pure(Nil)
    case h :: t => {
      Monad[M].bind(spanM(t)(p(h, _))) {
        case (x, y) =>
          Monad[M].map(groupByM(y)(p))((g: List[List[A]]) => (h :: x) :: g)
      }
    }
  }

  def mapAccumLeft[A, B, C](as: List[A])(c: C, f: (C, A) => (C, B)): (C, List[B]) = as match {
    case Nil    => (c, Nil)
    case h :: t => {
      val (i, j) = f(c, h)
      val (k, l) = mapAccumLeft(t)(i, f)
      (k, j :: l)
    }
  }

  def mapAccumRight[A, B, C](as: List[A])(c: C, f: (C, A) => (C, B)): (C, List[B]) = as match {
    case Nil    => (c, Nil)
    case h :: t => {
      val (i, j) = mapAccumRight(t)(c, f)
      val (k, l) = f(i, h)
      (k, l :: j)
    }
  }

  def tailz[A](as: List[A]): List[List[A]] = as match {
    case Nil           => scala.List(Nil)
    case xxs@(_ :: xs) => xxs :: tailz(xs)
  }

  def initz[A](as: List[A]): List[List[A]] = as match {
    case Nil           => scala.List(Nil)
    case xxs@(x :: xs) => Nil :: (initz(xs) map (x :: _))
  }

  def allPairs[A](as: List[A]): List[(A, A)] =
    tailz(as).tail flatMap (as zip _)

  def adjacentPairs[A](as: List[A]): List[(A, A)] = as match {
    case Nil      => Nil
    case (_ :: t) => as zip t
  }
}

object list extends Lists
