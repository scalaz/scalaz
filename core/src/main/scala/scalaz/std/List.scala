package scalaz
package std

import scalaz.Maybe.just

import scala.annotation.tailrec

trait ListInstances0 {
  implicit def listEqual[A](implicit A0: Equal[A]): Equal[List[A]] = new ListEqual[A] {
    implicit def A = A0
  }
}

trait ListInstances extends ListInstances0 {
  implicit val listInstance: Traverse[List] with MonadPlus[List] with Alt[List] with BindRec[List] with Zip[List] with Unzip[List] with Align[List] with IsEmpty[List] with Cobind[List] =
    new Traverse[List] with MonadPlus[List] with Alt[List] with IterableBindRec[List] with Zip[List] with Unzip[List] with Align[List] with IsEmpty[List] with Cobind[List] with IterableSubtypeFoldable[List] with Functor.OverrideWiden[List] {

      override def point[A](a: => A): List[A] =
        List(a)

      override def bind[A, B](fa: List[A])(f: A => List[B]): List[B] =
        fa flatMap f

      override def createNewBuilder[A]() =
        List.newBuilder[A]

      override def isEmpty[A](fa: List[A]): Boolean =
        fa.isEmpty

      override def plus[A](a: List[A], b: => List[A]): List[A] =
        a ++ b

      override def alt[A](a: => List[A], b: => List[A]): List[A] =
        plus(a, b)

      override def empty[A]: List[A] =
        Nil

      override def unzip[A, B](a: List[(A, B)]): (List[A], List[B]) =
        a.unzip

      override def zip[A, B](a: => List[A], b: => List[B]): List[(A, B)] = {
        val _a = a
        if(_a.isEmpty) empty
        else _a zip b
      }

      override def findLeft[A](fa: List[A])(f: A => Boolean) = fa.find(f)
      override def findRight[A](fa: List[A])(f: A => Boolean) = {
        @tailrec def loop(a: List[A], x: Option[A]): Option[A] =
          a match {
            case h :: t =>
              loop(t, if(f(h)) Some(h) else x)
            case Nil =>
              x
          }
        loop(fa, None)
      }

      def alignWith[A, B, C](f: A \&/ B => C) = {
        @annotation.tailrec
        def loop(aa: List[A], bb: List[B], accum: List[C]): List[C] = (aa, bb) match {
          case (Nil, _) =>
            accum reverse_::: bb.map(b => f(\&/.That(b)))
          case (_, Nil) =>
            accum reverse_::: aa.map(a => f(\&/.This(a)))
          case (ah :: at, bh :: bt) =>
            loop(at, bt, f(\&/.Both(ah, bh)) :: accum)
        }
        (a, b) => loop(a, b, Nil)
      }
      def traverseImpl[F[_], A, B](l: List[A])(f: A => F[B])(implicit F: Applicative[F]) = {
        val revOpt: Maybe[F[List[B]]] =
          F.unfoldrOpt[List[A], B, List[B]](l){
            case a :: as => just((f(a), as))
            case Nil => Maybe.empty
          }(Reducer.ReverseListReducer[B])

        val rev: F[List[B]] = revOpt getOrElse F.point(Nil)

        F.map(rev)(_.reverse)
      }

      override def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B) = {
        import scala.collection.mutable.ArrayStack
        val s = new ArrayStack[A]
        fa.foreach(a => s push a)
        var r = z
        while (!s.isEmpty) {
          // force and copy the value of r to ensure correctness
          val w = r
          r = f(s.pop(), w)
        }
        r
      }

      override def foldMap[A, B](fa: List[A])(f: A => B)(implicit M: Monoid[B]) =
        M.unfoldrSum(fa)(as => as.headOption match {
          case Some(a) => just((f(a), as.tail))
          case None => Maybe.empty
        })

      def cobind[A, B](fa: List[A])(f: List[A] => B) =
        fa match {
          case Nil => Nil
          case _::t => f(fa) :: cobind(t)(f)
        }

      override def cojoin[A](a: List[A]) =
        a match {
          case Nil => Nil
          case _::t => a :: cojoin(t)
        }
    }

  implicit def listMonoid[A]: Monoid[List[A]] = listInstance.monoid[A]

  implicit def listShow[A](implicit A: Show[A]): Show[List[A]] = Show.show { as =>
    import scalaz.syntax.show._
    val content = Foldable[List].intercalate(as.map(A.show), Cord(","))
    cord"[$content]"
  }

  implicit def listOrder[A](implicit A0: Order[A]): Order[List[A]] = new ListOrder[A] {
    implicit def A = A0
  }
}

trait ListFunctions {
  /** Intersperse the element `a` between each adjacent pair of elements in `as` */
  final def intersperse[A](as: List[A], a: A): List[A] = {
    @tailrec
    def intersperse0(accum: List[A], rest: List[A]): List[A] = rest match {
      case Nil      => accum
      case x :: Nil => x :: accum
      case h :: t   => intersperse0(a :: h :: accum, t)
    }
    intersperse0(Nil, as).reverse
  }

  final def tailOption[A](as: List[A]): Option[List[A]] = as match {
    case Nil    => None
    case _ :: t => Some(t)
  }

  /** [[scala.Nil]] with a sometimes more convenient type */
  final def nil[A]: List[A] = Nil

  final def toNel[A](as: List[A]): Maybe[NonEmptyList[A]] = as match {
    case Nil    => Maybe.empty
    case h :: t => just(NonEmptyList.nel(h, IList.fromList(t)))
  }

  final def toZipper[A](as: List[A]): Maybe[Zipper[A]] =
    lazylist.toZipper(as.to(LazyList))

  final def zipperEnd[A](as: List[A]): Maybe[Zipper[A]] =
    lazylist.zipperEnd(as.to(LazyList))

  /**
   * Returns `f` applied to the contents of `as` if non-empty, otherwise, the zero element of the `Monoid` for the type `B`.
   */
  final def <^>[A, B: Monoid](as: List[A])(f: NonEmptyList[A] => B): B = as match {
    case Nil    => Monoid[B].zero
    case h :: t => f(NonEmptyList.nel(h, IList.fromList(t)))
  }

  /** Run `p(a)`s and collect `as` while `p` yields true.  Don't run
    * any `p`s after the first false.
    */
  final def takeWhileM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[List[A]] = as match {
    case Nil    => Monad[M].point(Nil)
    case h :: t => Monad[M].bind(p(h))(b =>
      if (b) Monad[M].map(takeWhileM(t)(p))((tt: List[A]) => h :: tt) else Monad[M].point(Nil))
  }

  /** Run `p(a)`s and collect `as` while `p` yields false.  Don't run
    * any `p`s after the first true.
    */
  final def takeUntilM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[List[A]] =
    takeWhileM(as)((a: A) => Monad[M].map(p(a))((b) => !b))

  final def filterM[A, M[_] : Applicative](as: List[A])(p: A => M[Boolean]): M[List[A]] =
    Applicative[M].filterM(as)(p)

  /** Run `p(a)`s left-to-right until it yields a true value,
    * answering `Some(that)`, or `None` if nothing matched `p`.
    */
  final def findM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[Maybe[A]] = as match {
    case Nil    => Monad[M].point(Maybe.empty[A])
    case h :: t => Monad[M].bind(p(h))(b =>
      if (b) Monad[M].point(just[A](h)) else findM(t)(p))
  }

  final def powerset[A](as: List[A]): List[List[A]] = {
    import list.listInstance

    filterM(as)(_ => true :: false :: Nil)
  }

  /** A pair of passing and failing values of `as` against `p`. */
  final def partitionM[A, M[_]](as: List[A])(p: A => M[Boolean])(implicit F: Applicative[M]): M[(List[A], List[A])] = as match {
    case Nil    => F.point((Nil: List[A], Nil: List[A]))
    case h :: t =>
      F.ap(partitionM(t)(p))(F.map(p(h))(b => {
          case (x, y) => if (b) (h :: x, y) else (x, h :: y)
      }))
  }

  /** A pair of the longest prefix of passing `as` against `p`, and
    * the remainder. */
  final def spanM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[(List[A], List[A])] = as match {
    case Nil    => Monad[M].point((Nil, Nil))
    case h :: t =>
      Monad[M].bind(p(h))(b =>
        if (b) Monad[M].map(spanM(t)(p))((k: (List[A], List[A])) => (h :: k._1, k._2))
        else Monad[M].point((Nil, as)))

  }

  /** `spanM` with `p`'s complement. */
  final def breakM[A, M[_] : Monad](as: List[A])(p: A => M[Boolean]): M[(List[A], List[A])] =
    spanM(as)(a => Monad[M].map(p(a))((b: Boolean) => !b))

  /** Split at each point where `p(as(n), as(n+1))` yields false. */
  final def groupWhenM[A, M[_] : Monad](as: List[A])(p: (A, A) => M[Boolean]): M[List[NonEmptyList[A]]] = as match {
    case Nil    => Monad[M].point(Nil)
    case h :: t =>
      val stateP = (i: A) => StateT[A, M, Boolean](s => Monad[M].map(p(s, i))(i -> _))
      Monad[M].bind(spanM[A, StateT[A, M, *]](t)(stateP).eval(h)) {
        case (x, y) =>
          Monad[M].map(groupWhenM(y)(p))(g => NonEmptyList.nel(h, IList.fromList(x)) :: g)
      }
  }

  /** As with the standard library `groupBy` but preserving the fact that the values in the Map must be non-empty  */
  final def groupBy1[A, B](as: List[A])(f: A => B): Map[B, NonEmptyList[A]] = as.foldLeft(Map.empty[B, NonEmptyList[A]]) { (nels, a) =>
    val b = f(a)
    nels + (b -> (nels get b map (a <:: _) getOrElse NonEmptyList(a)))
  } map { case (k, v) => k -> v.reverse }

  /** `groupWhenM` specialized to [[scalaz.Id.Id]]. */
  final def groupWhen[A](as: List[A])(p: (A, A) => Boolean): List[NonEmptyList[A]] = {
    @tailrec
    def span1(xs: List[A], s: A, l: List[A]): (List[A], List[A]) = xs match {
      case Nil    => (l, Nil)
      case h :: t => if (p(s, h)) span1(t, h, h :: l) else (l, xs)
    }
    @tailrec
    def go(xs: List[A], acc: List[NonEmptyList[A]]): List[NonEmptyList[A]] = xs match {
      case Nil    => acc.reverse
      case h :: t =>
        val (x, y) = span1(t, h, Nil)
        go(y, NonEmptyList.nel(h, IList.fromList(x.reverse)) :: acc)
    }
    go(as, Nil)
  }

  private[this] def mapAccum[A, B, C](as: List[A])(c: C, f: (C, A) => (C, B)): (C, List[B]) =
    as.foldLeft((c, Nil: List[B])){ case ((c, bs), a) =>
      val (c0, b) = f(c, a)
      (c0, b :: bs)
    }

  /** All of the `B`s, in order, and the final `C` acquired by a
    * stateful left fold over `as`. */
  final def mapAccumLeft[A, B, C](as: List[A])(c: C, f: (C, A) => (C, B)): (C, List[B]) = {
    val (c0, list) = mapAccum(as)(c, f)
    (c0, list.reverse)
  }

  /** All of the `B`s, in order `as`-wise, and the final `C` acquired
    * by a stateful right fold over `as`. */
  final def mapAccumRight[A, B, C](as: List[A])(c: C, f: (C, A) => (C, B)): (C, List[B]) =
    mapAccum(as.reverse)(c, f)

  /** `[as, as.tail, as.tail.tail, ..., Nil]` */
  final def tailz[A](as: List[A]): List[List[A]] = as match {
    case Nil           => Nil :: Nil
    case xxs@(_ :: xs) => xxs :: tailz(xs)
  }

  /** `[Nil, as take 1, as take 2, ..., as]` */
  final def initz[A](as: List[A]): List[List[A]] = as match {
    case Nil           => Nil :: Nil
    case xxs@(x :: xs) => Nil :: (initz(xs) map (x :: _))
  }

  /** Combinations of `as` and `as`, excluding same-element pairs. */
  final def allPairs[A](as: List[A]): List[(A, A)] =
    tailz(as).tail flatMap (as zip _)

  /** `[(as(0), as(1)), (as(1), as(2)), ... (as(size-2), as(size-1))]` */
  final def adjacentPairs[A](as: List[A]): List[(A, A)] = as match {
    case Nil      => Nil
    case (_ :: t) => as zip t
  }
}

object list extends ListInstances with ListFunctions {
  object listSyntax extends scalaz.syntax.std.ToListOps
}


private trait ListEqual[A] extends Equal[List[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(a1: List[A], a2: List[A]) = (a1 corresponds a2)(Equal[A].equal)
}

private trait ListOrder[A] extends Order[List[A]] with ListEqual[A] {
  implicit def A: Order[A]

  import Ordering._

  @annotation.tailrec
  final def order(a1: List[A], a2: List[A]) =
    (a1, a2) match {
      case (Nil, Nil)     => EQ
      case (Nil, _::_)    => LT
      case (_::_, Nil)    => GT
      case (a::as, b::bs) => Order[A].order(a, b) match {
        case EQ => order(as, bs)
        case x  => x
      }
    }

}
