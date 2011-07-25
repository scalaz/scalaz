package scalaz
package wrap

sealed trait StreamW[A] {
  val value: Stream[A]

  import StreamW._
  import newtypes.ZipStream
  import Zipper._
  import iteratee._, IterateeT._, Input._
  import *._

  def ʐ : ZipStream[A] =
    value.*-->[ZipStream[A]]

  def merge(s: Stream[A]): Stream[A] = {
    if (value.isEmpty) s
    else value.head #:: s.merge(value.tail)
  }

  def toZipper: Option[Zipper[A]] = value match {
    case Stream.Empty => None
    case h #:: t => Some(zipper(Stream.Empty, h, t))
  }

  def zipperEnd: Option[Zipper[A]] = value match {
    case Stream.Empty => None
    case _ => {
      val x = value.reverse
      Some(zipper(x.tail, x.head, Stream.Empty))
    }
  }

  def heads: Stream[Stream[A]] = value match {
    case h #:: t => Stream(h) #:: t.heads.map(h #:: _)
    case _ => Stream.Empty
  }

  def tails: Stream[Stream[A]] = value match {
    case h #:: t => value #:: (t: StreamW[A]).tails
    case _ => Stream.Empty
  }

  def zapp[B, C](f: Stream[A => B => C]): Stream[B => C] = {
    val ff = f.value
    val aa = value
    if (ff.isEmpty || aa.isEmpty) Stream.empty
    else Stream.cons((ff.head)(aa.head), aa.tail.zapp(ff.tail))
  }

  def unfoldForest[B](f: A => (B, () => Stream[A])): Stream[Tree[B]] =
    value.map(a => {
      def unfoldTree(x: A): Tree[B] =
        f(x) match {
          case (b, bs) => Tree.node(b, bs().unfoldForest(f))
        }

      unfoldTree(a)
    })

  def unfoldForestM[B, M[_] : Monad](f: A => M[(B, Stream[A])]): M[Stream[Tree[B]]] = {
    def mapM[T, U](ts: Stream[T], f: T => M[U]): M[Stream[U]] =
      ts.foldRight[M[Stream[U]]](implicitly[Monad[M]].point(Stream())) {
        case (g, h) => implicitly[Monad[M]].liftM2((x: U) => (xs: Stream[U]) => x #:: xs)(f(g))(h)
      }

    val unfoldTreeM: A => M[Tree[B]] =
      (v: A) =>
        implicitly[Monad[M]].bd((abs: (B, Stream[A])) =>
          implicitly[Monad[M]].fmap((ts: Stream[Tree[B]]) =>
            Tree.node(abs._1, ts))(abs._2.unfoldForestM[B, M](f)))(f(v))

    mapM(value, unfoldTreeM)
  }

  def enumerate[O]: (A >@@> O) =
    i =>
      value match {
        case Stream.Empty => i
        case x #:: xs => i.fold(done = (_, _) =>  i, cont = k => xs.enumerate(k(elInput(x)).value.value), err = e => err[Unit, A, Identity, O](e).value.value)
      }
}

object StreamW extends StreamWs

trait StreamWs {
  implicit def StreamTo[A](as: Stream[A]): StreamW[A] = new StreamW[A] {
    val value = as
  }
}
