package scalaz
package std


trait LazyListInstances {
  implicit val lazylistInstance: Traverse[LazyList] with MonadPlus[LazyList] with BindRec[LazyList] with Zip[LazyList] with Unzip[LazyList] with Align[LazyList] with IsEmpty[LazyList] with Cobind[LazyList] = new Traverse[LazyList] with MonadPlus[LazyList] with BindRec[LazyList] with Zip[LazyList] with Unzip[LazyList] with Align[LazyList] with IsEmpty[LazyList] with Cobind[LazyList] {
    override def cojoin[A](a: LazyList[A]) = a.tails.to(LazyList).init
    def cobind[A, B](fa: LazyList[A])(f: LazyList[A] => B): LazyList[B] = map(cojoin(fa))(f)
    def traverseImpl[G[_], A, B](fa: LazyList[A])(f: A => G[B])(implicit G: Applicative[G]): G[LazyList[B]] = {
      val seed: G[LazyList[B]] = G.point(LazyList[B]())

      foldRight(fa, seed) {
        (x, ys) => G.apply2(f(x), ys)((b, bs) => b #:: bs)
      }
    }

    override def length[A](fa: LazyList[A]) = fa.length
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
      this.foldRight(fa, M.zero)((a, b) => M.append(f(a), b))

    override def foldMap1Opt[A, B](fa: LazyList[A])(f: A => B)(implicit B: Semigroup[B]) =
      foldMapRight1Opt(fa)(f)((l, r) => B.append(f(l), r))

    override def foldMapRight1Opt[A, B](fa: LazyList[A])(z: A => B)(f: (A, => B) => B): Option[B] = {
      def rec(hd: A, tl: => LazyList[A]): B = {
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

    override def toStream[A](fa: LazyList[A]) = fa.toStream

    override def zipWithL[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (A, Option[B]) => C) =
      if (fa.isEmpty) {
        LazyList.empty
      } else {
        val bTail = if(fb.isEmpty) LazyList.empty else fb.tail
        LazyList.cons(f(fa.head, fb.headOption), zipWithL(fa.tail, bTail)(f))
      }

    override def zipWithR[A, B, C](fa: LazyList[A], fb: LazyList[B])(f: (Option[A], B) => C) =
      zipWithL(fb, fa)((b, a) => f(a, b))

    override def filter[A](fa: LazyList[A])(p: A => Boolean): LazyList[A] = fa filter p

    override def map[A, B](fa: LazyList[A])(f: A => B): LazyList[B] = fa map f

    import Liskov.<~<
    override def widen[A, B](fa: LazyList[A])(implicit ev: A <~< B): LazyList[B] = Liskov.co(ev)(fa)

    def bind[A, B](fa: LazyList[A])(f: A => LazyList[B]) = fa flatMap f
    def empty[A]: LazyList[A] = LazyList.empty[A]
    def plus[A](a: LazyList[A], b: => LazyList[A]) = a #::: b
    def isEmpty[A](s: LazyList[A]) = s.isEmpty
    def point[A](a: => A) = LazyList(a)
    def zip[A, B](a: => LazyList[A], b: => LazyList[B]) = {
      val _a = a
      if(_a.isEmpty) LazyList.empty
      else _a zip b
    }
    def unzip[A, B](a: LazyList[(A, B)]) = a.unzip

    def alignWith[A, B, C](f: A \&/ B => C): (LazyList[A], LazyList[B]) => LazyList[C] =
      (a, b) =>
        if(b.isEmpty)
          a.map(x => f(\&/.This(x)))
        else if(a.isEmpty)
          b.map(x => f(\&/.That(x)))
        else
          f(\&/.Both(a.head, b.head)) #:: alignWith(f)(a.tail, b.tail)

    def tailrecM[A, B](f: A => LazyList[A \/ B])(a: A): LazyList[B] = {
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

  implicit def lazylistMonoid[A]: Monoid[LazyList[A]] = new Monoid[LazyList[A]] {
    def append(f1: LazyList[A], f2: => LazyList[A]) = f1 #::: f2
    def zero: LazyList[A] = LazyList.empty[A]
  }

  implicit def lazylistEqual[A](implicit A0: Equal[A]): Equal[LazyList[A]] =
    new LazyListEqual[A] { def A = A0 }
  implicit def lazylistOrder[A](implicit A0: Order[A]): Order[LazyList[A]] =
    new Order[LazyList[A]] with LazyListEqual[A] {
      override def A: Order[A] = A0
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
  implicit def lazylistShow[A](implicit A0: Show[A]): Show[LazyList[A]] =
    new Show[LazyList[A]] {
      override def show(as: LazyList[A]) = "LazyList(" +: lazylist.intersperse(as.map(A0.show), Cord(",")).foldLeft(Cord())(_ ++ _) :+ ")"
    }


}

trait LazyListFunctions {
  final def interleave[A](s1: LazyList[A], s2: LazyList[A]): LazyList[A] =
    if (s1.isEmpty) s2
    else s1.head #:: interleave(s2, s1.tail)

  import LazyList.empty

  final def toZipper[A](as: LazyList[A]): Option[Zipper[A]] =
    as match {
      case h #:: t => Some(Zipper.zipper(Stream.empty, h, t.toStream))
      case _ => None
    }

  final def zipperEnd[A](as: LazyList[A]): Option[Zipper[A]] =
    scalaz.std.stream.zipperEnd(as.toStream)


  /** `[as take 1, as take 2, ..., as]` */
  final def heads[A](as: LazyList[A]): LazyList[LazyList[A]] =
    as match {
      case h #:: t => LazyList(h) #:: heads(t).map(h #:: _)
      case _       => empty
    }

  /** `[as, as.tail, as.tail.tail, ..., LazyList(as.last)]` */
  final def tails[A](as: LazyList[A]): LazyList[LazyList[A]] =
    as match {
      case h #:: t => as #:: tails(t)
      case _       => empty
    }

  final def zapp[A, B, C](a: LazyList[A])(f: LazyList[A => B => C]): LazyList[B => C] = {
    val ff = f
    val aa = a
    if (ff.isEmpty || aa.isEmpty) empty
    else scala.LazyList.cons((ff.head)(aa.head), zapp(aa.tail)(ff.tail))
  }

  final def unfoldForest[A, B](as: LazyList[A])(f: A => (B, () => LazyList[A])): LazyList[Tree[B]] =
    as.map(a => {
      def unfoldTree(x: A): Tree[B] =
        f(x) match {
          case (b, bs) => Tree.Node(b, unfoldForest(bs())(f).toStream)
        }

      unfoldTree(a)
    })

  final def unfoldForestM[A, B, M[_]](as: LazyList[A])(f: A => M[(B, LazyList[A])])(implicit M: Monad[M]): M[LazyList[Tree[B]]] = {
    M.map(
      scalaz.std.stream.unfoldForestM[A, B, M](
        as.toStream
      )(f.andThen(M.map(_)(x => x.copy(_2 = x._2.toStream))))
    )(_.to(LazyList))
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
