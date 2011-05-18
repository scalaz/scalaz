package scalaz
package wrap

sealed trait ListW[A] {

  import ListW._
  import StreamW._
  import annotation.tailrec
  import data.{Zipper, NonEmptyList}

  val value: List[A]

  def intersperse(a: A): List[A] = {
    @tailrec
    def intersperse0(accum: List[A], rest: List[A]): List[A] = rest match {
      case Nil => accum
      case x :: Nil => x :: accum
      case h :: t => intersperse0(a :: h :: accum, t)
    }
    intersperse0(nil, value) reverse
  }

  def intercalate(as: List[A]): List[A] = {
    val asr = as reverse
    @tailrec
    def intercalate0(accum: List[A], rest: List[A]): List[A] = rest match {
      case Nil => accum
      case x :: Nil => x :: accum
      case h :: t => intercalate0(asr ::: h :: accum, t)
    }
    intercalate0(nil, value) reverse
  }

  def toNel: Option[NonEmptyList[A]] = value match {
    case Nil => None
    case h :: t => Some(Scalaz.nel(h, t))
  }

  def toZipper: Option[Zipper[A]] =
    value.toStream.toZipper

  def zipperEnd: Option[Zipper[A]] =
    value.toStream.zipperEnd

  def <^>[B: Zero](f: NonEmptyList[A] => B): B = value match {
    case Nil => implicitly[Zero[B]].zero
    case h :: t => f(NonEmptyList.nel(h, t))
  }

  def stripPrefix(prefix: List[A]): Option[List[A]] = {
    val (before, after) = value splitAt prefix.length
    if (before == prefix) Some(after) else None
  }

  def takeWhileM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = value match {
    case Nil => implicitly[Monad[M]].point(nil[A])
    case h :: t => implicitly[Monad[M]].bd((b: Boolean) =>
      if (b) t takeWhileM p else implicitly[Monad[M]].point(nil[A]))(p(h))
  }

  def takeUntilM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] =
    takeWhileM((a: A) => implicitly[Monad[M]].fmap((b: Boolean) => !b)(p(a)))

  def filterM[M[_] : Monad](p: A => M[Boolean]): M[List[A]] = value match {
    case Nil => implicitly[Monad[M]].point(nil[A])
    case h :: t => {
      def g = t filterM p
      implicitly[Monad[M]].bd((b: Boolean) =>
        if (b) implicitly[Monad[M]].fmap((tt: List[A]) => h :: tt)(g) else g)(p(h))
    }
  }

  def powerset: List[List[A]] = filterM(_ => List(true, false))

  def partitionM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = value match {
    case Nil => implicitly[Monad[M]].point(nil[A], nil[A])
    case h :: t =>
      implicitly[Monad[M]].bd((b: Boolean) =>
        implicitly[Monad[M]].fmap((xy: (List[A], List[A])) =>
          if (b) (h :: xy._1, xy._2) else (xy._1, h :: xy._2))(t partitionM p))(p(h))
  }

  def spanM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] = value match {
    case Nil => implicitly[Monad[M]].point(nil[A], nil[A])
    case h :: t =>
      implicitly[Monad[M]].bd((b: Boolean) =>
        if (b) implicitly[Monad[M]].fmap((k: (List[A], List[A])) =>
          (h :: k._1, k._2))(t spanM p)
        else
          implicitly[Monad[M]].point(nil[A], value))(p(h))
  }

  def breakM[M[_] : Monad](p: A => M[Boolean]): M[(List[A], List[A])] =
    spanM(a => implicitly[Monad[M]].fmap((b: Boolean) => !b)(p(a)))

  def groupByM[M[_] : Monad](p: (A, A) => M[Boolean]): M[List[List[A]]] = value match {
    case Nil => implicitly[Monad[M]].point(nil[List[A]])
    case h :: t => {
      implicitly[Monad[M]].bd((xy: (List[A], List[A])) =>
        implicitly[Monad[M]].fmap((g: List[List[A]]) =>
          (h :: xy._1) :: g)(xy._2 groupByM p))(t.spanM(p(h, _)))
    }
  }

  def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, List[B]) = value match {
    case Nil => (c, Nil)
    case h :: t => {
      val (i, j) = f(c, h)
      val (k, l) = t.mapAccumLeft(i, f)
      (k, j :: l)
    }
  }

  def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, List[B]) = value match {
    case Nil => (c, Nil)
    case h :: t => {
      val (i, j) = t.mapAccumRight(c, f)
      val (k, l) = f(i, h)
      (k, l :: j)
    }
  }

  def tailz: List[List[A]] = value match {
    case Nil => List(Nil)
    case xxs@(_ :: xs) => xxs :: xs.tailz
  }

  def initz: List[List[A]] = value match {
    case Nil => List(Nil)
    case xxs@(x :: xs) => Nil :: (xs.initz map (x :: _))
  }

  def allPairs: List[(A, A)] =
    value.tailz.tail flatMap (value zip _)

  def adjacentPairs: List[(A, A)] = value match {
    case Nil => Nil
    case (_ :: t) => value zip t
  }
}

object ListW extends ListWs

trait ListWs {
  implicit def ListTo[A](as: List[A]): ListW[A] = new ListW[A] {
    val value = as
  }

  def nil[A]: List[A] = Nil
}
