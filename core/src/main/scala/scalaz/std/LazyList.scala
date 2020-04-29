package scalaz
package std

import Maybe.just

trait LazyListInstances {
  implicit val lazylistInstance: Traverse[LazyList] with MonadPlus[LazyList] with Alt[LazyList] with BindRec[LazyList] with Zip[LazyList] with Unzip[LazyList] with Align[LazyList] with IsEmpty[LazyList] with Cobind[LazyList] = new Traverse[LazyList] with MonadPlus[LazyList] with Alt[LazyList] with BindRec[LazyList] with Zip[LazyList] with Unzip[LazyList] with Align[LazyList] with IsEmpty[LazyList] with Cobind[LazyList] with IterableSubtypeFoldable[LazyList] with Functor.OverrideWiden[LazyList] {

    override def point[A](a: => A): LazyList[A] =
      LazyList(a)

    override def bind[A, B](fa: LazyList[A])(f: A => LazyList[B]): LazyList[B] =
      fa flatMap f

    override def isEmpty[A](fa: LazyList[A]): Boolean =
      fa.isEmpty

    override def empty[A]: LazyList[A] =
      LazyList.empty[A]

    override def unzip[A, B](a: LazyList[(A, B)]): (LazyList[A], LazyList[B]) =
      a.unzip

    override def zip[A, B](a: => LazyList[A],b: => LazyList[B]): LazyList[(A, B)] = {
      val _a = a
      if(_a.isEmpty) empty
      else _a zip b
    }

    override def cojoin[A](a: LazyList[A]) = a.tails.to(LazyList).init
    def cobind[A, B](fa: LazyList[A])(f: LazyList[A] => B): LazyList[B] = map(cojoin(fa))(f)

    override def index[A](fa: LazyList[A], i: Int) = {
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

    override def foldLeft[A, B](fa: LazyList[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)

    override def foldMapLeft1Opt[A, B](fa: LazyList[A])(z: A => B)(f: (B, A) => B): Option[B] = fa match {
      case hd #:: tl => Some(tl.foldLeft(z(hd))(f))
      case _ => None
    }

    override def foldMap[A, B](fa: LazyList[A])(f: A => B)(implicit M: Monoid[B]) =
      M.unfoldrSum(fa)(as => as.headOption match {
        case Some(a) => Maybe.just((f(a), as.tail))
        case None => Maybe.empty
      })

    override def foldMap1Opt[A, B](fa: LazyList[A])(f: A => B)(implicit B: Semigroup[B]) =
      foldMapRight1Opt(fa)(f)((l, r) => B.append(f(l), r))

    override def foldMapRight1Opt[A, B](fa: LazyList[A])(z: A => B)(f: (A, => B) => B): Option[B] = {
      def rec(hd: A, tl: LazyList[A]): B = {
        if (tl.isEmpty) {
          z(hd)
        } else {
          f(hd, rec(tl.head, tl.tail))
        }
      }
      fa match {
        case hd #:: tl => Some(rec(hd, tl))
        case _ => None
      }
    }

    override def foldRight[A, B](fa: LazyList[A], z: => B)(f: (A, => B) => B): B = if (fa.isEmpty)
      z
    else
      f(fa.head, foldRight(fa.tail, z)(f))

    override def zipWithL[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (A, Option[B]) => C) =
      if(fa.isEmpty) LazyList.empty
      else {
        val bTail = if(fb.isEmpty) LazyList.empty else fb.tail
        LazyList.cons(f(fa.head, fb.headOption), zipWithL(fa.tail, bTail)(f))
      }

    override def zipWithR[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (Option[A], B) => C) =
      zipWithL(fb, fa)((b, a) => f(a, b))

    def plus[A](a: LazyList[A], b: => LazyList[A]) = a #::: b

    def alt[A](a: => LazyList[A], b: => LazyList[A]) = plus(a, b)

    def alignWith[A, B, C](f: A \&/ B => C): (LazyList[A], LazyList[B]) => LazyList[C] =
      (a, b) =>
        if(b.isEmpty)
          a.map(x => f(\&/.This(x)))
        else if(a.isEmpty)
          b.map(x => f(\&/.That(x)))
        else
          f(\&/.Both(a.head, b.head)) #:: alignWith(f)(a.tail, b.tail)

    def tailrecM[A, B](a: A)(f: A => LazyList[A \/ B]): LazyList[B] = {
      def go(s: LazyList[A \/ B]): LazyList[B] = {
        @annotation.tailrec def rec(abs: LazyList[A \/ B]): LazyList[B] =
          abs match {
            case \/-(b) #:: tail => b #:: go(tail)
            case -\/(a) #:: tail => rec(f(a) #::: tail)
            case _ => LazyList.empty
          }
        rec(s)
      }
      go(f(a))
    }

    def traverseImpl[F[_], A, B](fa: LazyList[A])(f: A => F[B])(implicit F: Applicative[F]) = {
      val revOpt: Maybe[F[List[B]]] =
        F.unfoldrOpt[LazyList[A], B, List[B]](fa)(_ match {
          case a #:: as => Maybe.just((f(a), as))
          case _ => Maybe.empty
        })(Reducer.ReverseListReducer[B])

      val rev: F[List[B]] = revOpt getOrElse F.point(Nil)

      F.map(rev)((rev) => rev.foldLeft(LazyList[B]())((r, c) => c +: r))
    }
  }

  import Tags.Zip

  /**
   * An alternative [[scalaz.Applicative]] instance for `LazyList`, discriminated by the type tag [[scalaz.Tags.Zip]],
   * that zips lazylists together.
   *
   * Example:
   * {{{
   * import scalaz.Tags.Zip
   * lazylistZipApplicative.apply2(Zip(LazyList(1, 2)), Zip(LazyList(3, 4)))(_ * _) // LazyList(3, 8)
   * }}}
   */
  implicit val lazylistZipApplicative: Applicative[λ[α => LazyList[α] @@ Zip]] =
    new Applicative[λ[α => LazyList[α] @@ Zip]] {
      def point[A](a: => A) = Zip(LazyList.continually(a))
      def ap[A, B](fa: => (LazyList[A] @@ Zip))(f: => (LazyList[A => B] @@ Zip)) = {
        Zip(if (Tag.unwrap(f).isEmpty || Tag.unwrap(fa).isEmpty) LazyList.empty[B]
        else LazyList.cons((Tag.unwrap(f).head)(Tag.unwrap(fa).head), Tag.unwrap(ap(Zip(Tag.unwrap(fa).tail))(Zip(Tag.unwrap(f).tail)))))
      }
    }

  implicit def lazylistMonoid[A]: Monoid[LazyList[A]] = lazylistInstance.monoid[A]

  implicit def lazylistEqual[A](implicit A0: Equal[A]): Equal[LazyList[A]] =
    new LazyListEqual[A] { def A = A0 }
  implicit def lazylistOrder[A](implicit A0: Order[A]): Order[LazyList[A]] =
    new Order[LazyList[A]] with LazyListEqual[A] {
      def A = A0
      import Ordering._
      @annotation.tailrec
      override final def order(a: LazyList[A], b: LazyList[A]): Ordering =
        if(a.isEmpty) {
          if(b.isEmpty) EQ
          else LT
        } else {
          if(b.isEmpty) GT
          else {
            A.order(a.head, b.head) match {
              case EQ => order(a.tail, b.tail)
              case x => x
            }
          }
        }
    }
  implicit def lazylistShow[A](implicit A0: Show[A]): Show[LazyList[A]] = Show.show { as =>
    import scalaz.syntax.show._
    val content = Foldable[LazyList].intercalate(as.map(A0.show), Cord(","))
    cord"LazyList($content)"
  }

}

trait LazyListFunctions {
  final def interleave[A](s1: LazyList[A], s2: LazyList[A]): LazyList[A] =
    if (s1.isEmpty) s2
    else s1.head #:: interleave(s2, s1.tail)

  final def toZipper[A](as: LazyList[A]): Maybe[Zipper[A]] =
    as match {
      case h #:: t => just(Zipper.zipper(Stream.empty, h, t.toStream))
      case _ => Maybe.empty
    }

  final def zipperEnd[A](as: LazyList[A]): Maybe[Zipper[A]] =
    if (as.isEmpty) {
      Maybe.empty
    } else {
      val x = as.reverse
      just(Zipper.zipper(x.tail.toStream, x.head, Stream.empty))
    }

  /** `[as take 1, as take 2, ..., as]` */
  final def heads[A](as: LazyList[A]): LazyList[LazyList[A]] =
    as match {
      case h #:: t => LazyList(h) #:: heads(t).map(h #:: _)
      case _       => LazyList.empty
    }

  /** `[as, as.tail, as.tail.tail, ..., LazyList(as.last)]` */
  final def tails[A](as: LazyList[A]): LazyList[LazyList[A]] =
    as match {
      case h #:: t => as #:: tails(t)
      case _       => LazyList.empty
    }

  final def zapp[A, B, C](a: LazyList[A])(f: LazyList[A => B => C]): LazyList[B => C] = {
    val ff = f
    val aa = a
    if (ff.isEmpty || aa.isEmpty) LazyList.empty
    else LazyList.cons((ff.head)(aa.head), zapp(aa.tail)(ff.tail))
  }

  final def unfoldForest[A, B](as: LazyList[A])(f: A => (B, () => LazyList[A])): LazyList[Tree[B]] =
    as.map(a => {
      def unfoldTree(x: A): Tree[B] =
        f(x) match {
          case (b, bs) => Tree.Node(b, EphemeralStream.fromStream(unfoldForest(bs())(f).toStream))
        }

      unfoldTree(a)
    })

  final def unfoldForestM[A, B, M[_] : Monad](as: LazyList[A])(f: A => M[(B, LazyList[A])]): M[LazyList[Tree[B]]] = {
    def mapM[T, U](ts: LazyList[T], f: T => M[U]): M[LazyList[U]] =
      ts.foldRight[M[LazyList[U]]](Monad[M].point(scala.LazyList())) {
        case (g, h) => Monad[M].apply2(f(g), h)(_ #:: _)
      }

    def unfoldTreeM(v: A) =
      Monad[M].bind(f(v))((abs: (B, LazyList[A])) =>
        Monad[M].map(unfoldForestM[A, B, M](abs._2)(f))((ts: LazyList[Tree[B]]) =>
          Tree.Node(abs._1, EphemeralStream.fromStream(ts.toStream))))

    mapM(as, unfoldTreeM)
  }

  /** Intersperse the element `a` between each adjacent pair of elements in `as` */
  final def intersperse[A](as: LazyList[A], a: A): LazyList[A] = {
    def loop(rest: LazyList[A]): LazyList[A] = rest match {
      case h #:: t => a #:: h #:: loop(t)
      case _ => LazyList.empty
    }
    as match {
      case h #:: t => h #:: loop(t)
      case _ => LazyList.empty
    }
  }

  def unfold[A, B](seed: A)(f: A => Option[(B, A)]): LazyList[B] =
    f(seed) match {
      case None         => LazyList.empty
      case Some((b, a)) => LazyList.cons(b, unfold(a)(f))
    }

  def unfoldm[A, B](seed: A)(f: A => Maybe[(B, A)]): LazyList[B] =
    f(seed) match {
      case Maybe.Empty() => LazyList.empty
      case Maybe.Just((b, a))  => LazyList.cons(b, unfoldm(a)(f))
    }
}

object lazylist extends LazyListInstances with LazyListFunctions {
  object lazylistSyntax extends scalaz.syntax.std.ToLazyListOps
}

private trait LazyListEqual[A] extends Equal[LazyList[A]] {
  def A: Equal[A]
  override final def equal(a1: LazyList[A], a2: LazyList[A]) = (a1 corresponds a2)(A.equal)
}
