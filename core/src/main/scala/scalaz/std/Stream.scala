package scalaz
package std

trait StreamInstances {
  implicit object streamInstance extends Traverse[Stream] with MonadPlus[Stream] with Each[Stream] with Index[Stream] with Length[Stream] {
    def traverseImpl[G[_] : Applicative, A, B](fa: Stream[A])(f: (A) => G[B]): G[Stream[B]] = {
      val G = Applicative[G]
      val seed: G[Stream[B]] = G.point(scala.Stream.empty[B])
      foldR(fa, seed) {
        x => ys =>
          G.ap(ys)(G.map(f(x))((a: B) => (b: Stream[B]) => a #:: b))
      }
    }

    def each[A](fa: Stream[A])(f: (A) => Unit) = fa foreach f
    def length[A](fa: Stream[A]): Int = fa.length
    def index[A](fa: Stream[A], i: Int): Option[A] = {
      var n = 0
      var k: Option[A] = None
      val it = fa.iterator
      while (it.hasNext && k.isEmpty) {
        val z = it.next
        if (n == i) k = Some(z)
        n = n + 1
      }

      k
    }

    def foldRight[A, B](fa: Stream[A], z: => B)(f: (A, => B) => B): B = if (fa.isEmpty)
      z
    else
      f(fa.head, foldRight(fa.tail, z)(f))

    def bind[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa flatMap f
    def empty[A]: Stream[A] = scala.Stream.empty
    def plus[A](a: Stream[A], b: => Stream[A]): Stream[A] = a #::: b
    def point[A](a: => A): Stream[A] = scala.Stream(a)
  }

  implicit def streamMonoid[A] = new Monoid[Stream[A]] {
    def append(f1: Stream[A], f2: => Stream[A]): Stream[A] = f1 #::: f2
    def zero: Stream[A] = scala.Stream.empty
  }

  // TODO show, equal, order, ...
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
        case (g, h) => Monad[M].lift2((x: U, xs: Stream[U]) => x #:: xs)(f(g), h)
      }

    def unfoldTreeM(v: A) =
      Monad[M].bind(f(v))((abs: (B, Stream[A])) =>
        Monad[M].map(unfoldForestM[A, B, M](abs._2)(f))((ts: Stream[Tree[B]]) =>
          Tree.node(abs._1, ts)))

    mapM(as, unfoldTreeM)
  }
}

object stream extends StreamInstances with StreamFunctions {
  object streamSyntax extends scalaz.syntax.std.ToStreamV
}