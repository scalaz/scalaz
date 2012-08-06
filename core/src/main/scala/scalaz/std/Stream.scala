package scalaz
package std

import annotation.tailrec

trait StreamInstances {
  implicit val streamInstance: Traverse[Stream] with MonadPlus[Stream] with Each[Stream] with Index[Stream] with Length[Stream] with Zip[Stream] with Unzip[Stream] = new Traverse[Stream] with MonadPlus[Stream] with Each[Stream] with Index[Stream] with Length[Stream] with Zip[Stream] with Unzip[Stream] {
    def traverseImpl[G[_], A, B](fa: Stream[A])(f: (A) => G[B])(implicit G: Applicative[G]): G[Stream[B]] = {
      val seed: G[Stream[B]] = G.point(Stream[B]())

      foldRight(fa, seed) {
        (x, ys) => G.map2(f(x), ys)((b, bs) => b #:: bs)
      }
    }

    def each[A](fa: Stream[A])(f: (A) => Unit) = fa foreach f
    def length[A](fa: Stream[A]) = fa.length
    def index[A](fa: Stream[A], i: Int) = {
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

    override def foldRight[A, B](fa: Stream[A], z: => B)(f: (A, => B) => B): B = if (fa.isEmpty)
      z
    else
      f(fa.head, foldRight(fa.tail, z)(f))

    def bind[A, B](fa: Stream[A])(f: (A) => Stream[B]) = fa flatMap f
    def empty[A]: Stream[A] = scala.Stream.empty
    def plus[A](a: Stream[A], b: => Stream[A]) = a #::: b
    def point[A](a: => A) = scala.Stream(a)
    def zip[A, B](a: => Stream[A], b: => Stream[B]) = a zip b
    def unzip[A, B](a: Stream[(A, B)]) = a.unzip

  }

  import Tags.Zip

  /**
   * An alternative [[scalaz.Applicative]] instance for `Stream`, discriminated by the type tag [[scalaz.Tags.Zip]],
   * that zips streams together.
   *
   * Example:
   * {{{
   * streamZipApplicative.map2(Stream(1, 2), Stream(3, 4))(_ * _) // Stream(3, 8)
   * }}}
   */
  implicit val streamZipApplicative: Applicative[({type λ[α]=Stream[α] @@ Zip})#λ] = new Applicative[({type λ[α]=Stream[α] @@ Zip})#λ] {
    def point[A](a: => A) = Zip(Stream.continually(a))
    def ap[A, B](fa: => (Stream[A] @@ Zip))(f: => (Stream[A => B] @@ Zip)) = {
      Zip(if (f.isEmpty || fa.isEmpty) Stream.empty[B]
      else Stream.cons((f.head)(fa.head), ap(Zip(fa.tail))(Zip(f.tail))))
    }
  }

  implicit def streamMonoid[A] = new Monoid[Stream[A]] {
    def append(f1: Stream[A], f2: => Stream[A]) = f1 #::: f2
    def zero: Stream[A] = scala.Stream.empty
  }

  implicit def streamEqual[A](implicit A0: Equal[A]) = new Equal[Stream[A]] {
    def equal(a1: Stream[A], a2: Stream[A]) = (a1 corresponds a2)(A0.equal)
  }
  implicit def streamShow[A](implicit A0: Show[A]) = new Show[Stream[A]] {
    def show(as: Stream[A]) = {
      val i = as.iterator
      val k = new collection.mutable.ListBuffer[Char]
      k += '['
      while (i.hasNext) {
        val n = i.next()
        k ++= Show[A].show(n)
        if (i.hasNext)
          k += ','
      }
      k += ']'
      k.toList
    }
  }


  // TODO order, ...
}

trait StreamFunctions {
  final def merge[A](s1: Stream[A], s2: Stream[A]): Stream[A] = {
    if (s1.isEmpty) s2
    else s1.head #:: merge(s2, s1.tail)
  }

  import scala.Stream.{Empty, empty}

  final def toZipper[A](as: Stream[A]): Option[Zipper[A]] = as match {
    case Empty   => None
    case h #:: t => Some(Zipper.zipper(empty, h, t))
  }

  final def zipperEnd[A](as: Stream[A]): Option[Zipper[A]] = as match {
    case Empty => None
    case _     =>
      val x = as.reverse
      Some(Zipper.zipper(x.tail, x.head, empty))
  }

  final def heads[A](as: Stream[A]): Stream[Stream[A]] = as match {
    case h #:: t => scala.Stream(h) #:: heads(t).map(h #:: _)
    case _       => empty
  }

  final def tails[A](as: Stream[A]): Stream[Stream[A]] = as match {
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
          case (b, bs) => Tree.node(b, unfoldForest(bs())(f))
        }

      unfoldTree(a)
    })

  final def unfoldForestM[A, B, M[_] : Monad](as: Stream[A])(f: A => M[(B, Stream[A])]): M[Stream[Tree[B]]] = {
    def mapM[T, U](ts: Stream[T], f: T => M[U]): M[Stream[U]] =
      ts.foldRight[M[Stream[U]]](Monad[M].point(scala.Stream())) {
        case (g, h) => Monad[M].map2(f(g), h)(_ #:: _)
      }

    def unfoldTreeM(v: A) =
      Monad[M].bind(f(v))((abs: (B, Stream[A])) =>
        Monad[M].map(unfoldForestM[A, B, M](abs._2)(f))((ts: Stream[Tree[B]]) =>
          Tree.node(abs._1, ts)))

    mapM(as, unfoldTreeM)
  }
}

object stream extends StreamInstances with StreamFunctions {
  object streamSyntax extends scalaz.syntax.std.ToStreamOps
}
