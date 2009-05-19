package scalaz

sealed trait StreamW[A] {
  val value: Stream[A]

  import Scalaz._
  import MA._

  def string(f: A => Char) = value map f mkString

  def stringj(f: A => Stream[Char]) = value flatMap f mkString

  def |!| = ZipStream.zip(value)

  def merge(s: Stream[A]): Stream[A] =
    if (value.isEmpty) Stream.empty
    else Stream.cons(value.head, s.merge(value.tail))

  def zipper = value match {
    case Stream.empty => None
    case Stream.cons(h, t) => Some(Scalaz.zipper(Stream.empty, h, t))
  }

  def zipperEnd = value match {
    case Stream.empty => None
    case _ => {
      val x = value.reverse
      Some(Scalaz.zipper(x.tail, x.head, Stream.empty))
    }
  }

  def heads: Stream[Stream[A]] = value match {
    case Stream.cons(h, t) => Stream.cons(Stream(h), t.heads.map(Stream.cons(h, _)))
    case _ => Stream.empty
  }

  def tails: Stream[Stream[A]] = value match {
    case Stream.cons(h, t) => Stream.cons(value, t.tails)
    case _ => Stream.empty
  }

  def zapp[B, C](fs: ZipStream[A => B => C]) = (value |!|) <*> fs

  def unfoldForest[B](f: A => (B, () => Stream[A])): Stream[Tree[B]] = value.map(_.unfoldTree(f))

  import Traverse._
  def unfoldForestM[B, M[_]](f: A => M[(B, Stream[A])])(implicit m: Monad[M]): M[Stream[Tree[B]]] =
    value.traverse[M].apply[Tree[B]]((_: A).unfoldTreeM[B, M](f))
}

object StreamW {
  implicit def StreamTo[A](as: Stream[A]): StreamW[A] = new StreamW[A] {
    val value = as
  }

  implicit def StreamFrom[A](as: StreamW[A]): Stream[A] = as.value

}
