package scalaz
package std

import vector._
import annotation.tailrec

sealed trait VectorInstances0 {
  implicit def vectorEqual[A](implicit A0: Equal[A]): Equal[Vector[A]] = new VectorEqual[A] {
    implicit def A = A0
  }
}

trait VectorInstances extends VectorInstances0 {
  implicit val vectorInstance: Traverse[Vector] with MonadPlus[Vector] with BindRec[Vector] with Zip[Vector] with Unzip[Vector] with IsEmpty[Vector] with Align[Vector] = new Traverse[Vector] with MonadPlus[Vector] with BindRec[Vector] with Zip[Vector] with Unzip[Vector] with IsEmpty[Vector] with Align[Vector] {
    override def index[A](fa: Vector[A], i: Int) = fa.lift.apply(i)
    override def length[A](fa: Vector[A]) = fa.length
    def point[A](a: => A) = empty :+ a
    def bind[A, B](fa: Vector[A])(f: A => Vector[B]) = fa flatMap f
    def empty[A] = Vector.empty[A]
    def plus[A](a: Vector[A], b: => Vector[A]) = a ++ b
    def isEmpty[A](a: Vector[A]) = a.isEmpty
    override def map[A, B](v: Vector[A])(f: A => B) = v map f
    override def filter[A](fa: Vector[A])(p: A => Boolean): Vector[A] = fa filter p

    def zip[A, B](a: => Vector[A], b: => Vector[B]): Vector[(A, B)] = {
      val _a = a
      if(_a.isEmpty) empty
      else _a zip b
    }
    def unzip[A, B](a: Vector[(A, B)]) = a.unzip

    def traverseImpl[F[_], A, B](v: Vector[A])(f: A => F[B])(implicit F: Applicative[F]) = {
      v.foldLeft(F.point(empty[B])) { (fvb, a) =>
        F.apply2(fvb, f(a))(_ :+ _)
      }
    }

    override def traverseS[S,A,B](v: Vector[A])(f: A => State[S,B]): State[S,Vector[B]] =
      State((s: S) =>
        v.foldLeft((s, empty[B]))((acc, a) => {
          val bs = f(a)(acc._1)
          (bs._1, acc._2 :+ bs._2)
        }))

    override def toVector[A](fa: Vector[A]) = fa

    override def foldRight[A, B](fa: Vector[A], z: => B)(f: (A, => B) => B) = {
      var i = fa.length
      var r = z
      while (i > 0) {
        i -= 1
        // force and copy the value of r to ensure correctness
        val w = r
        r = f(fa(i), w)
      }
      r
    }

    def tailrecM[A, B](f: A => Vector[A \/ B])(a: A): Vector[B] = {
      val bs = Vector.newBuilder[B]
      @scala.annotation.tailrec
      def go(xs: List[Vector[A \/ B]]): Unit =
        xs match {
          case Vector(\/-(b), tail @ _*) :: rest =>
            bs += b
            go(tail.toVector :: rest)
          case Vector(-\/(a0), tail @ _*) :: rest =>
            go(f(a0) :: tail.toVector :: rest)
          case Vector() :: rest =>
            go(rest)
          case Nil =>
        }
      go(List(f(a)))
      bs.result
    }

    def alignWith[A, B, C](f: A \&/ B => C): (Vector[A], Vector[B]) => Vector[C] = { (as, bs) =>
      val sizeA = as.size
      val sizeB = bs.size
      (as, bs).zipped.map((a, b) => f(\&/.Both(a, b))) ++ {
        if(sizeA > sizeB)
          as.drop(sizeB).map(a => f(\&/.This(a)))
        else
          bs.drop(sizeA).map(b => f(\&/.That(b)))
      }
    }

    override def all[A](fa: Vector[A])(f: A => Boolean) =
      fa forall f

    override def any[A](fa: Vector[A])(f: A => Boolean) =
      fa exists f
  }

  implicit def vectorMonoid[A]: Monoid[Vector[A]] = new Monoid[Vector[A]] {
    // Vector concat is O(n^2) in Scala 2.10 - it's actually faster to do repeated appends
    // https://issues.scala-lang.org/browse/SI-7725
    //
    // It was reduced to O(n) in Scala 2.11 - ideally it would be O(log n)
    // https://issues.scala-lang.org/browse/SI-4442
    def append(f1: Vector[A], f2: => Vector[A]) = f2.foldLeft(f1)(_ :+ _)
    def zero: Vector[A] = Vector.empty
  }

  implicit def vectorShow[A: Show]: Show[Vector[A]] = new Show[Vector[A]] {
    import Cord._
    override def show(as: Vector[A]) =
      Cord("[", mkCord(",", as.map(Show[A].show(_)):_*), "]")
  }

  implicit def vectorOrder[A](implicit A0: Order[A]): Order[Vector[A]] = new VectorOrder[A] {
    implicit def A = A0
  }
}

trait VectorFunctions {
  protected def empty[A]: Vector[A] = Vector.empty

  @inline private[this] final
  def lazyFoldRight[A, B](as: Vector[A], b: => B)(f: (A, => B) => B) = {
    def rec(ix: Int): B =
      if (ix >= as.length - 1) b else f(as(ix+1), rec(ix+1))
    rec(-1)
  }

  /** Intersperse the element `a` between each adjacent pair of elements in `as` */
  final def intersperse[A](as: Vector[A], a: A): Vector[A] =
    if (as.isEmpty) empty else as.init.foldRight(as.last +: empty)(_ +: a +: _)

  final def toNel[A](as: Vector[A]): Option[NonEmptyList[A]] =
    if (as.isEmpty) None else Some(NonEmptyList.nel(as.head, IList.fromFoldable(as.tail)))

  final def toZipper[A](as: Vector[A]): Option[Zipper[A]] =
    stream.toZipper(as.toStream)

  final def zipperEnd[A](as: Vector[A]): Option[Zipper[A]] =
    stream.zipperEnd(as.toStream)

  /**
   * Returns `f` applied to the contents of `as` if non-empty, otherwise, the zero element of the `Monoid` for the type `B`.
   */
  final def <^>[A, B: Monoid](as: Vector[A])(f: NonEmptyList[A] => B): B =
    if (as.isEmpty) Monoid[B].zero else f(NonEmptyList.nel(as.head, IList.fromFoldable(as.tail)))

  /** Run `p(a)`s and collect `as` while `p` yields true.  Don't run
    * any `p`s after the first false.
    */
  final def takeWhileM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[Vector[A]] =
    lazyFoldRight(as, Monad[M].point(empty[A]))((a, as) =>
      Monad[M].bind(p(a))(b =>
        if (b) Monad[M].map(as)((tt: Vector[A]) => a +: tt)
        else Monad[M].point(empty)))

  /** Run `p(a)`s and collect `as` while `p` yields false.  Don't run
    * any `p`s after the first true.
    */
  final def takeUntilM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[Vector[A]] =
    takeWhileM(as)((a: A) => Monad[M].map(p(a))((b) => !b))

  final def filterM[A, M[_]](as: Vector[A])(p: A => M[Boolean])(implicit F: Applicative[M]): M[Vector[A]] =
    lazyFoldRight(as, F.point(empty[A]))((a, g) =>
      F.ap(g)(F.map(p(a))(b => t => if (b) a +: t else t)))

  /** Run `p(a)`s left-to-right until it yields a true value,
    * answering `Some(that)`, or `None` if nothing matched `p`.
    */
  final def findM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[Option[A]] =
    lazyFoldRight(as, Monad[M].point(None: Option[A]))((a, g) =>
      Monad[M].bind(p(a))(b =>
        if (b) Monad[M].point(Some(a): Option[A]) else g))

  final def powerset[A](as: Vector[A]): Vector[Vector[A]] = {
    import vector.vectorInstance
    val tf = empty[Boolean] :+ true :+ false
    filterM(as)(_ => tf)
  }

  /** A pair of passing and failing values of `as` against `p`. */
  final def partitionM[A, M[_]](as: Vector[A])(p: A => M[Boolean])(implicit F: Applicative[M]): M[(Vector[A], Vector[A])] =
    lazyFoldRight(as, F.point(empty[A], empty[A]))((a, g) =>
      F.ap(g)(F.map(p(a))(b => {
        case (x, y) => if (b) (a +: x, y) else (x, a +: y)
      })))

  /** A pair of the longest prefix of passing `as` against `p`, and
    * the remainder. */
  final def spanM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[(Vector[A], Vector[A])] =
    Monad[M].map(takeWhileM(as)(p))(ys => (ys, as drop (ys.length)))

  /** `spanM` with `p`'s complement. */
  final def breakM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[(Vector[A], Vector[A])] =
    spanM(as)(a => Monad[M].map(p(a))((b: Boolean) => !b))

  /** Split at each point where `p(as(n), as(n+1))` yields false. */
  final def groupWhenM[A, M[_] : Monad](as: Vector[A])(p: (A, A) => M[Boolean]): M[Vector[Vector[A]]] =
    if (as.isEmpty)
      Monad[M].point(empty)
    else {
      val stateP = (i: A) => StateT[M, A, Boolean](s => Monad[M].map(p(s, i))(i ->))
      Monad[M].bind(spanM[A, StateT[M, A, ?]](as.tail)(stateP).eval(as.head)) {
        case (x, y) =>
          Monad[M].map(groupWhenM(y)(p))((g: Vector[Vector[A]]) => (as.head +: x) +: g)
      }
    }

  /** `groupWhenM` specialized to [[scalaz.Id.Id]]. */
  final def groupWhen[A](as: Vector[A])(p: (A, A) => Boolean): Vector[Vector[A]] = {
    @tailrec
    def span1(xs: Vector[A], s: A, l: Vector[A]): (Vector[A], Vector[A]) =
      if (xs.isEmpty) (l, empty)
      else {
        val h = xs.head
        val t = xs.tail
        if (p(s, h)) span1(t, h, l :+ h) else (l, xs)
      }
    @tailrec
    def go(xs: Vector[A], acc: Vector[Vector[A]]): Vector[Vector[A]] = {
      if(xs.isEmpty)
        acc
      else {
        val (x, y) = span1(xs.tail, xs.head, empty)
        go(y, acc :+ (xs.head +: x))
      }
    }
    go(as, empty)
  }

  /** All of the `B`s, in order, and the final `C` acquired by a
    * stateful left fold over `as`. */
  final def mapAccumLeft[A, B, C](as: Vector[A])(c: C, f: (C, A) => (C, B)): (C, Vector[B]) =
    as.foldLeft((c, empty[B])){(acc, a) => acc match {
      case (c, v) => f(c, a) match {
        case (c, b) => (c, v :+ b)
      }}
    }

  /** All of the `B`s, in order `as`-wise, and the final `C` acquired
    * by a stateful right fold over `as`. */
  final def mapAccumRight[A, B, C](as: Vector[A])(c: C, f: (C, A) => (C, B)): (C, Vector[B]) =
    as.foldRight((c, empty[B])){(a, acc) => acc match {
      case (c, v) => f(c, a) match {
        case (c, b) => (c, b +: v)
      }}
    }

  /** `[as, as.tail, as.tail.tail, ..., `empty Vector`]` */
  final def tailz[A](as: Vector[A]): Vector[Vector[A]] =
    if (as.isEmpty) empty[A] +: empty else as +: tailz(as.tail)

  /** `[`empty Vector`, as take 1, as take 2, ..., as]` */
  final def initz[A](as: Vector[A]): Vector[Vector[A]] = {
    @tailrec def rec(acc: Vector[Vector[A]], as: Vector[A]): Vector[Vector[A]] =
      if (as.isEmpty) as +: acc else rec(as +: acc, as.init)
    rec(empty, as)
  }

  /** Combinations of `as` and `as`, excluding same-element pairs. */
  final def allPairs[A](as: Vector[A]): Vector[(A, A)] =
    tailz(as).tail flatMap (as zip _)

  /** `[(as(0), as(1)), (as(1), as(2)), ... (as(size-2), as(size-1))]` */
  final def adjacentPairs[A](as: Vector[A]): Vector[(A, A)] =
    if (as.isEmpty) empty else as zip as.tail
}

private trait VectorEqual[A] extends Equal[Vector[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: Vector[A], a2: Vector[A]) = (a1 corresponds a2)(Equal[A].equal)
}

private trait VectorOrder[A] extends Order[Vector[A]] with VectorEqual[A] {
  implicit def A: Order[A]

  import Ordering._
  import scalaz.std.anyVal._

  def order(a1: Vector[A], a2: Vector[A]): Ordering = {
    val a1s = a1.length
    @tailrec def receqs(ix: Int): Ordering =
      if (ix >= a1s) EQ else A.order(a1(ix), a2(ix)) match {
        case EQ => receqs(ix + 1)
        case o => o
      }
    Semigroup[Ordering].append(Order[Int].order(a1s, a2.length),
                               receqs(0))
  }
}

object vector extends VectorInstances with VectorFunctions {
  object vectorSyntax extends scalaz.syntax.std.ToVectorOps
}
