package scalaz
package std


trait StreamInstances {
  implicit val streamInstance: Traverse[Stream] with MonadPlus[Stream] with BindRec[Stream] with Zip[Stream] with Unzip[Stream] with Align[Stream] with IsEmpty[Stream] with Cobind[Stream] = new Traverse[Stream] with MonadPlus[Stream] with BindRec[Stream] with Zip[Stream] with Unzip[Stream] with Align[Stream] with IsEmpty[Stream] with Cobind[Stream] {
    override def cojoin[A](a: Stream[A]) = a.tails.toStream.init
    def cobind[A, B](fa: Stream[A])(f: Stream[A] => B): Stream[B] = map(cojoin(fa))(f)
    def traverseImpl[G[_], A, B](fa: Stream[A])(f: A => G[B])(implicit G: Applicative[G]): G[Stream[B]] = {
      val seed: G[Stream[B]] = G.point(Stream[B]())

      foldRight(fa, seed) {
        (x, ys) => G.apply2(f(x), ys)((b, bs) => b #:: bs)
      }
    }

    override def length[A](fa: Stream[A]) = fa.length
    override def index[A](fa: Stream[A], i: Int) = {
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

    override def foldLeft[A, B](fa: Stream[A], z: B)(f: (B, A) => B): B = fa.foldLeft(z)(f)

    override def foldMapLeft1Opt[A, B](fa: Stream[A])(z: A => B)(f: (B, A) => B): Option[B] = fa match {
      case Stream.Empty => None
      case hd #:: tl => Some(tl.foldLeft(z(hd))(f))
    }

    override def foldMap[A, B](fa: Stream[A])(f: A => B)(implicit M: Monoid[B]) =
      this.foldRight(fa, M.zero)((a, b) => M.append(f(a), b))

    override def foldMap1Opt[A, B](fa: Stream[A])(f: A => B)(implicit B: Semigroup[B]) =
      foldMapRight1Opt(fa)(f)((l, r) => B.append(f(l), r))

    override def foldMapRight1Opt[A, B](fa: Stream[A])(z: A => B)(f: (A, => B) => B): Option[B] = {
      def rec(hd: A, tl: Stream[A]): B = tl match {
        case Stream.Empty => z(hd)
        case h #:: t => f(hd, rec(h, t))
      }
      fa match {
        case Stream.Empty => None
        case hd #:: tl => Some(rec(hd, tl))
      }
    }

    override def foldRight[A, B](fa: Stream[A], z: => B)(f: (A, => B) => B): B = if (fa.isEmpty)
      z
    else
      f(fa.head, foldRight(fa.tail, z)(f))

    override def toStream[A](fa: Stream[A]) = fa

    override def zipWithL[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, Option[B]) => C) =
      if(fa.isEmpty) Stream.Empty
      else {
        val bTail = if(fb.isEmpty) Stream.Empty else fb.tail
        Stream.cons(f(fa.head, fb.headOption), zipWithL(fa.tail, bTail)(f))
      }

    override def zipWithR[A, B, C](fa: Stream[A], fb: Stream[B])(f: (Option[A], B) => C) =
      zipWithL(fb, fa)((b, a) => f(a, b))

    override def filter[A](fa: Stream[A])(p: A => Boolean): Stream[A] = fa filter p

    def bind[A, B](fa: Stream[A])(f: A => Stream[B]) = fa flatMap f
    def empty[A]: Stream[A] = scala.Stream.empty
    def plus[A](a: Stream[A], b: => Stream[A]) = a #::: b
    def isEmpty[A](s: Stream[A]) = s.isEmpty
    def point[A](a: => A) = scala.Stream(a)
    def zip[A, B](a: => Stream[A], b: => Stream[B]) = {
      val _a = a
      if(_a.isEmpty) Stream.Empty
      else _a zip b
    }
    def unzip[A, B](a: Stream[(A, B)]) = a.unzip

    def alignWith[A, B, C](f: A \&/ B => C): (Stream[A], Stream[B]) => Stream[C] =
      (a, b) =>
        if(b.isEmpty)
          a.map(x => f(\&/.This(x)))
        else if(a.isEmpty)
          b.map(x => f(\&/.That(x)))
        else
          f(\&/.Both(a.head, b.head)) #:: alignWith(f)(a.tail, b.tail)

    def tailrecM[A, B](f: A => Stream[A \/ B])(a: A): Stream[B] = {
      def go(s: Stream[A \/ B]): Stream[B] = {
        @annotation.tailrec def rec(abs: Stream[A \/ B]): Stream[B] = 
          abs match {
            case \/-(b) #:: tail => b #:: go(tail)
            case -\/(a) #:: tail => rec(f(a) #::: tail)
            case Stream.Empty => Stream.Empty
          }
        rec(s)
      }
      go(f(a))
    }
  }

  import Tags.Zip

  /**
   * An alternative [[scalaz.Applicative]] instance for `Stream`, discriminated by the type tag [[scalaz.Tags.Zip]],
   * that zips streams together.
   *
   * Example:
   * {{{
   * import scalaz.Tags.Zip
   * streamZipApplicative.apply2(Zip(Stream(1, 2)), Zip(Stream(3, 4)))(_ * _) // Stream(3, 8)
   * }}}
   */
  implicit val streamZipApplicative: Applicative[λ[α => Stream[α] @@ Zip]] =
    new Applicative[λ[α => Stream[α] @@ Zip]] {
      def point[A](a: => A) = Zip(Stream.continually(a))
      def ap[A, B](fa: => (Stream[A] @@ Zip))(f: => (Stream[A => B] @@ Zip)) = {
        Zip(if (Tag.unwrap(f).isEmpty || Tag.unwrap(fa).isEmpty) Stream.empty[B]
        else Stream.cons((Tag.unwrap(f).head)(Tag.unwrap(fa).head), Tag.unwrap(ap(Zip(Tag.unwrap(fa).tail))(Zip(Tag.unwrap(f).tail)))))
      }
    }

  implicit def streamMonoid[A] = new Monoid[Stream[A]] {
    def append(f1: Stream[A], f2: => Stream[A]) = f1 #::: f2
    def zero: Stream[A] = scala.Stream.empty
  }

  implicit def streamEqual[A](implicit A0: Equal[A]): Equal[Stream[A]] =
    new StreamEqual[A] { def A = A0 }
  implicit def streamOrder[A](implicit A0: Order[A]): Order[Stream[A]] =
    new Order[Stream[A]] with StreamEqual[A] {
      def A = A0
      import Ordering._
      @annotation.tailrec
      override final def order(a: Stream[A], b: Stream[A]): Ordering =
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
  implicit def streamShow[A](implicit A0: Show[A]) =
    new Show[Stream[A]] {
      override def show(as: Stream[A]) = "Stream(" +: stream.intersperse(as.map(A0.show), Cord(",")).foldLeft(Cord())(_ ++ _) :+ ")"
    }


}

trait StreamFunctions {
  final def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] =
    if (s1.isEmpty) s2
    else s1.head #:: interleave(s2, s1.tail)

  import scala.Stream.{Empty, empty}

  final def toZipper[A](as: Stream[A]): Option[Zipper[A]] =
    as match {
      case Empty   => None
      case h #:: t => Some(Zipper.zipper(empty, h, t))
    }

  final def zipperEnd[A](as: Stream[A]): Option[Zipper[A]] =
    as match {
      case Empty => None
      case _     =>
        val x = as.reverse
        Some(Zipper.zipper(x.tail, x.head, empty))
    }

  /** `[as take 1, as take 2, ..., as]` */
  final def heads[A](as: Stream[A]): Stream[Stream[A]] = 
    as match {
      case h #:: t => scala.Stream(h) #:: heads(t).map(h #:: _)
      case _       => empty
    }

  /** `[as, as.tail, as.tail.tail, ..., Stream(as.last)]` */
  final def tails[A](as: Stream[A]): Stream[Stream[A]] = 
    as match {
      case h #:: t => as #:: tails(t)
      case _       => empty
    }

  final def zapp[A, B, C](a: Stream[A])(f: Stream[A => B => C]): Stream[B => C] = {
    val ff = f
    val aa = a
    if (ff.isEmpty || aa.isEmpty) empty
    else scala.Stream.cons((ff.head)(aa.head), zapp(aa.tail)(ff.tail))
  }

  final def unfoldForest[A, B](as: Stream[A])(f: A => (B, () => Stream[A])): Stream[Tree[B]] =
    as.map(a => {
      def unfoldTree(x: A): Tree[B] =
        f(x) match {
          case (b, bs) => Tree.Node(b, unfoldForest(bs())(f))
        }

      unfoldTree(a)
    })

  final def unfoldForestM[A, B, M[_] : Monad](as: Stream[A])(f: A => M[(B, Stream[A])]): M[Stream[Tree[B]]] = {
    def mapM[T, U](ts: Stream[T], f: T => M[U]): M[Stream[U]] =
      ts.foldRight[M[Stream[U]]](Monad[M].point(scala.Stream())) {
        case (g, h) => Monad[M].apply2(f(g), h)(_ #:: _)
      }

    def unfoldTreeM(v: A) =
      Monad[M].bind(f(v))((abs: (B, Stream[A])) =>
        Monad[M].map(unfoldForestM[A, B, M](abs._2)(f))((ts: Stream[Tree[B]]) =>
          Tree.Node(abs._1, ts)))

    mapM(as, unfoldTreeM)
  }

  /** Intersperse the element `a` between each adjacent pair of elements in `as` */
  final def intersperse[A](as: Stream[A], a: A): Stream[A] = {
    def loop(rest: Stream[A]): Stream[A] = rest match {
      case Stream.Empty => Stream.empty
      case h #:: t      => a #:: h #:: loop(t)
    }
    as match {
      case Stream.Empty => Stream.empty
      case h #:: t      => h #:: loop(t)
    }
  }

  def unfold[A, B](seed: A)(f: A => Option[(B, A)]): Stream[B] =
    f(seed) match {
      case None         => Stream.empty
      case Some((b, a)) => Stream.cons(b, unfold(a)(f))
    }

  def unfoldm[A, B](seed: A)(f: A => Maybe[(B, A)]): Stream[B] =
    f(seed) match {
      case Maybe.Empty() => Stream.empty
      case Maybe.Just((b, a))  => Stream.cons(b, unfoldm(a)(f))
    }
}

object stream extends StreamInstances with StreamFunctions {
  object streamSyntax extends scalaz.syntax.std.ToStreamOps
}

private trait StreamEqual[A] extends Equal[Stream[A]] {
  def A: Equal[A]
  override final def equal(a1: Stream[A], a2: Stream[A]) = (a1 corresponds a2)(A.equal)
}
