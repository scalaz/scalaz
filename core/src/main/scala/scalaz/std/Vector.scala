package scalaz
package std

import annotation.tailrec

trait VectorInstances0 {
  implicit def vectorEqual[A](implicit A0: Equal[A]) = new VectorEqual[A] {
    implicit def A = A0
  }
}

trait VectorInstances extends VectorInstances0 {
  implicit val vectorInstance = new Traverse[Vector] with MonadPlus[Vector] with Each[Vector] with Index[Vector] with Length[Vector] with ApplicativePlus[Vector] with Zip[Vector] with Unzip[Vector] {
    def each[A](fa: Vector[A])(f: (A) => Unit) = fa foreach f
    def index[A](fa: Vector[A], i: Int) = if (fa.size > i) Some(fa(i)) else None
    def length[A](fa: Vector[A]) = fa.length
    def point[A](a: => A) = scala.Vector(a)
    def bind[A, B](fa: Vector[A])(f: A => Vector[B]) = fa flatMap f
    def empty[A] = scala.Vector()
    def plus[A](a: Vector[A], b: => Vector[A]) = a ++ b
    override def map[A, B](v: Vector[A])(f: A => B) = v map f

    def zip[A, B](a: => Vector[A], b: => Vector[B]) = a zip b
    def unzip[A, B](a: Vector[(A, B)]) = a.unzip

    def traverseImpl[F[_], A, B](v: Vector[A])(f: A => F[B])(implicit F: Applicative[F]) = {
      DList.fromList(v.toList).foldr(F.point(Vector[B]())) {
         (a, fbs) => F.map2(f(a), fbs)(_ +: _)
      }
    }
    
    override def traverseS[S,A,B](v: Vector[A])(f: A => State[S,B]): State[S,Vector[B]] =
      State((s: S) => 
        v.foldLeft((s, Vector[B]()))((acc, a) => {
          val bs = f(a)(acc._1)
          (bs._1, acc._2 :+ bs._2)
        }))

    override def foldRight[A, B](fa: Vector[A], z: => B)(f: (A, => B) => B) = {
      import scala.collection.mutable.ArrayStack
      val s = new ArrayStack[A]
      fa.foreach(a => s += a)
      var r = z
      while (!s.isEmpty) {
        // force and copy the value of r to ensure correctness
        val w = r
        r = f(s.pop, w)
      }
      r
    }

  }

  implicit def vectorMonoid[A]: Monoid[Vector[A]] = new Monoid[Vector[A]] {
    def append(f1: Vector[A], f2: => Vector[A]) = f1 ++ f2
    def zero: Vector[A] = Vector()
  }

  implicit def vectorShow[A: Show]: Show[Vector[A]] = new Show[Vector[A]] {
    def show(as: Vector[A]) =
      (List('[') +: (vector.intersperse(as.map(Show[A].show(_)), List(',')) :+ List(']'))).flatten.toList
  }

  implicit def vectorOrder[A](implicit A0: Order[A]): Order[Vector[A]] = new VectorOrder[A] {
    implicit def A = A0
  }

}

trait VectorFunctions {
  /** Intersperse the element `a` between each adjacent pair of elements in `as` */
  final def intersperse[A](as: Vector[A], a: A): Vector[A] = {
    @tailrec
    def intersperse0(accum: Vector[A], rest: Vector[A]): Vector[A] =
      if (rest.isEmpty) accum else if (rest.tail.isEmpty) rest.head +: accum else intersperse0(a +: rest.head +: accum, rest.tail)
    intersperse0(Vector(), as).reverse
  }

  final def intercalate[A](as1: Vector[Vector[A]], as2: Vector[A]): Vector[A] = intersperse(as1, as2).flatten

  final def toNel[A](as: Vector[A]): Option[NonEmptyList[A]] = 
    if (as.isEmpty) None else Some(NonEmptyList.nel(as.head, as.tail.toList))

  final def toZipper[A](as: Vector[A]): Option[Zipper[A]] =
    stream.toZipper(as.toStream)

  final def zipperEnd[A](as: Vector[A]): Option[Zipper[A]] =
    stream.zipperEnd(as.toStream)

  /**
   * Returns `f` applied to the contents of `as` if non-empty, otherwise, the zero element of the `Monoid` for the type `B`.
   */
  final def <^>[A, B: Monoid](as: Vector[A])(f: NonEmptyList[A] => B): B =
    if (as.isEmpty) Monoid[B].zero else f(NonEmptyList.nel(as.head, as.tail.toList))

  final def takeWhileM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[Vector[A]] =
    if (as.isEmpty) Monad[M].point(Vector()) else Monad[M].bind(p(as.head))(b =>
      if (b) Monad[M].map(takeWhileM(as.tail)(p))((tt: Vector[A]) => as.head +: tt) else Monad[M].point(Vector()))

  final def takeUntilM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[Vector[A]] =
    takeWhileM(as)((a: A) => Monad[M].map(p(a))((b) => !b))

  final def filterM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[Vector[A]] =
    if (as.isEmpty) Monad[M].point(Vector()) else {
      def g = filterM(as.tail)(p)
      Monad[M].bind(p(as.head))(b => if (b) Monad[M].map(g)(tt => as.head +: tt) else g)
    }
  
  final def findM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[Option[A]] =
    if (as.isEmpty) Monad[M].point(None: Option[A]) else Monad[M].bind(p(as.head))(b =>
      if (b) Monad[M].point(Some(as.head): Option[A]) else findM(as.tail)(p))

  final def powerset[A](as: Vector[A]): Vector[Vector[A]] = {
    import vector.vectorInstance

    filterM(as)(_ => Vector(true, false))
  }

  final def partitionM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[(Vector[A], Vector[A])] =
    if (as.isEmpty) Monad[M].point(Vector[A](), Vector[A]()) else
      Monad[M].bind(p(as.head))(b =>
        Monad[M].map(partitionM(as.tail)(p)) {
          case (x, y) => if (b) (as.head +: x, y) else (x, as.head +: y)
        }
      )

  final def spanM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[(Vector[A], Vector[A])] =
    if (as.isEmpty) Monad[M].point(Vector(), Vector()) else
      Monad[M].bind(p(as.head))(b =>
        if (b) Monad[M].map(spanM(as.tail)(p))((k: (Vector[A], Vector[A])) => (as.head +: k._1, k._2))
        else Monad[M].point(Vector(), as))

  final def breakM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[(Vector[A], Vector[A])] =
    spanM(as)(a => Monad[M].map(p(a))((b: Boolean) => !b))

  final def groupByM[A, M[_] : Monad](as: Vector[A])(p: (A, A) => M[Boolean]): M[Vector[Vector[A]]] =
    if (as.isEmpty) Monad[M].point(Vector()) else
      Monad[M].bind(spanM(as.tail)(p(as.head, _))) {
        case (x, y) =>
          Monad[M].map(groupByM(y)(p))((g: Vector[Vector[A]]) => (as.head +: x) +: g)
      }

  final def mapAccumLeft[A, B, C](as: Vector[A])(c: C, f: (C, A) => (C, B)): (C, Vector[B]) =
    if (as.isEmpty) (c, Vector()) else {
      val (i, j) = f(c, as.head)
      val (k, v) = mapAccumLeft(as.tail)(i, f)
      (k, j +: v)
    }

  final def mapAccumRight[A, B, C](as: Vector[A])(c: C, f: (C, A) => (C, B)): (C, Vector[B]) =
    if (as.isEmpty) (c, Vector()) else {
      val (i, j) = mapAccumRight(as.tail)(c, f)
      val (k, v) = f(i, as.head)
      (k, v +: j)
    }

  final def tailz[A](as: Vector[A]): Vector[Vector[A]] =
    if (as.isEmpty) Vector(Vector()) else as +: tailz(as.tail)

  final def initz[A](as: Vector[A]): Vector[Vector[A]] =
    if (as.isEmpty) Vector(Vector()) else Vector() +: (initz(as.tail) map (as.head +: _))

  final def allPairs[A](as: Vector[A]): Vector[(A, A)] =
    tailz(as).tail flatMap (as zip _)

  final def adjacentPairs[A](as: Vector[A]): Vector[(A, A)] =
    if (as.isEmpty) Vector() else as zip as.tail
}

object vector extends VectorInstances with VectorFunctions {
  object vectorSyntax extends scalaz.syntax.std.ToVectorOps
}

trait VectorEqual[A] extends Equal[Vector[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Vector[A], a2: Vector[A]) = (a1 corresponds a2)(Equal[A].equal)
}

trait VectorOrder[A] extends Order[Vector[A]] with VectorEqual[A] {
  implicit def A: Order[A]

  import Ordering._

  def order(a1: Vector[A], a2: Vector[A]) =
    (a1, a2) match {
      case (Vector(), Vector()) => EQ
      case (Vector(), y)        => LT
      case (x, Vector())        => GT
      case (as, bs) => Order[A].order(as.head, bs.head) match {
        case EQ => order(as.tail, bs.tail)
        case x  => x
      }
    }

}
