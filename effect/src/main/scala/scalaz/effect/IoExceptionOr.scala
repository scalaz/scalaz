package scalaz
package effect

sealed trait IoExceptionOr[A] {

  import IoExceptionOr._

  def fold[X](ioException: IoException => X, or: A => X): X

  def map[B](f: A => B): IoExceptionOr[B] =
    fold(ioException, a => IoExceptionOr(f(a)))

  def flatMap[B](f: A => IoExceptionOr[B]): IoExceptionOr[B] =
    fold(ioException, f)

  def forall(p: A => Boolean): Boolean =
    fold(_ => true, p)

  def exists(p: A => Boolean): Boolean =
    fold(_ => false, p)

  def toOption: Option[A] =
    fold(_ => None, Some(_))

  def valueOr(a: => A): A =
    fold(_ => a, x => x)
}

object IoExceptionOr extends IoExceptionOrFunctions {
  def apply[A](a: => A): IoExceptionOr[A] =
    try {
      ioExceptionOr(a)
    } catch {
      case e: java.io.IOException => ioException(e)
    }
  def unapply[A](ioExceptionOr: IoExceptionOr[A]) = ioExceptionOr.toOption
}

trait IoExceptionOrFunctions {
  type IoException =
  java.io.IOException

  def ioException[A]: IoException => IoExceptionOr[A] =
    e => new IoExceptionOr[A] {
      def fold[X](ioException: IoException => X, or: A => X) =
        ioException(e)
    }

  def ioExceptionOr[A](a: A): IoExceptionOr[A] = new IoExceptionOr[A] {
    def fold[X](ioException: IoException => X, or: A => X) =
      or(a)
  }

}
