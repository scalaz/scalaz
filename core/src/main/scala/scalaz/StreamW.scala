package scalaz

sealed trait StreamW[A] extends PimpedType[Stream[A]] {
  import Scalaz._

  def ʐ: ZipStream[A] = zip(value)

  def merge(s: Stream[A]): Stream[A] =
    if (value.isEmpty) s
    else value.head #:: s.merge(value.tail)

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
    case h #:: t => value #:: t.tails
    case _ => Stream.Empty
  }

  def zapp[B, C](fs: ZipStream[A => B => C]): ZipStream[B => C] = (value ʐ) <*> fs

  def unfoldForest[B](f: A => (B, () => Stream[A])): Stream[Tree[B]] = value.map(_.unfoldTree(f))

  def unfoldForestM[B, M[_]: Monad](f: A => M[(B, Stream[A])]): M[Stream[Tree[B]]] =
    value ↦ ((_: A).unfoldTreeM(f))
}

trait Streams {
  implicit def StreamTo[A](as: Stream[A]): StreamW[A] = new StreamW[A] {
    val value = as
  }
}
