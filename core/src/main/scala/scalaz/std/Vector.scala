package scalaz
package std

import vector._
import annotation.{switch, tailrec}
import Maybe.just

sealed trait VectorInstances0 {
  implicit def vectorEqual[A](implicit A0: Equal[A]): Equal[Vector[A]] = new VectorEqual[A] {
    implicit def A = A0
  }
}

trait VectorInstances extends VectorInstances0 {
  implicit val vectorInstance: Traverse[Vector] with MonadPlus[Vector] with Alt[Vector] with BindRec[Vector] with Zip[Vector] with Unzip[Vector] with IsEmpty[Vector] with Align[Vector] = new Traverse[Vector] with MonadPlus[Vector] with Alt[Vector] with IterableBindRec[Vector] with Zip[Vector] with Unzip[Vector] with IsEmpty[Vector] with Align[Vector] with IterableSubtypeFoldable[Vector] with Functor.OverrideWiden[Vector] {

    override def point[A](a: => A): Vector[A] =
      Vector(a)

    override def bind[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] =
      fa flatMap f

    override def createNewBuilder[A](): scala.collection.mutable.Builder[A, Vector[A]] =
      Vector.newBuilder[A]

    override def isEmpty[A](fa: Vector[A]): Boolean =
      fa.isEmpty

    override def plus[A](a: Vector[A], b: => Vector[A]): Vector[A] =
      a ++ b

    override def alt[A](a: => Vector[A], b: => Vector[A]): Vector[A] =
      plus(a, b)

    override def empty[A]: Vector[A] =
      Vector.empty[A]

    override def unzip[A, B](a: Vector[(A, B)]): (Vector[A], Vector[B]) =
      a.unzip

    override def zip[A, B](a: => Vector[A],b: => Vector[B]): Vector[(A, B)] = {
      val _a = a
      if(_a.isEmpty) empty
      else _a zip b
    }

    def traverseImpl[F[_], A, B](v: Vector[A])(f: A => F[B])(implicit F: Applicative[F]) = {
      // invariant: 0 <= s <= e <= v.size
      def rec(s: Int, e: Int): F[Vector[B]] = (e - s: @switch) match {
        case 0 => F.point(Vector())
        case 1 => F.map(f(v(s)))(Vector(_))
        // cases >1 but the inductive case are just optimizations; the
        // returns diminish pretty rapidly, so only '2' is here
        case 2 => F.apply2(f(v(s)), f(v(s + 1)))(Vector(_, _))
        case n =>
          val pivot = s + n / 2
          F.apply2(rec(s, pivot), rec(pivot, e))(_ ++ _)
      }
      rec(0, v.size)
    }

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
  }

  // Vector concat was reduced to O(n) in Scala 2.11 - ideally it would be O(log n)
  // https://issues.scala-lang.org/browse/SI-4442
  implicit def vectorMonoid[A]: Monoid[Vector[A]] = vectorInstance.monoid[A]

  implicit def vectorShow[A: Show]: Show[Vector[A]] = Show.show { as =>
    list.listShow[A].show(as.toList)
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

  final def toNel[A](as: Vector[A]): Maybe[NonEmptyList[A]] =
    if (as.isEmpty) Maybe.empty else just(NonEmptyList.nel(as.head, IList.fromFoldable(as.tail)))

  final def toZipper[A](as: Vector[A]): Maybe[Zipper[A]] =
    stream.toZipper(as.toStream)

  final def zipperEnd[A](as: Vector[A]): Maybe[Zipper[A]] =
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
    * answering `Maybe.Just(that)`, or `Maybe.empty` if nothing matched `p`.
    */
  final def findM[A, M[_] : Monad](as: Vector[A])(p: A => M[Boolean]): M[Maybe[A]] =
    lazyFoldRight(as, Monad[M].point(Maybe.empty[A]))((a, g) =>
      Monad[M].bind(p(a))(b =>
        if (b) Monad[M].point(just[A](a)) else g))

  final def powerset[A](as: Vector[A]): Vector[Vector[A]] = {
    import vector.vectorInstance
    val tf = empty[Boolean] :+ true :+ false
    filterM(as)(_ => tf)
  }

  /** A pair of passing and failing values of `as` against `p`. */
  final def partitionM[A, M[_]](as: Vector[A])(p: A => M[Boolean])(implicit F: Applicative[M]): M[(Vector[A], Vector[A])] =
    lazyFoldRight(as, F.point((empty[A], empty[A])))((a, g) =>
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
      val stateP = (i: A) => StateT[A, M, Boolean](s => Monad[M].map(p(s, i))(i -> _))
      Monad[M].bind(spanM[A, StateT[A, M, *]](as.tail)(stateP).eval(as.head)) {
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
