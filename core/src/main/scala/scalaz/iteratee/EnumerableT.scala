package scalaz
package iteratee

import IterateeT._
import EnumerateeT._
import Input._
import effect._
import java.io.{InputStream, BufferedInputStream, Reader, BufferedReader}

sealed trait EnumerableT[X, E, F[_], A] {
  val apply: X => EnumerateeT[E, F, A]

  import EnumerableT._

  def contramap[Y](f: Y => X): EnumerableT[Y, E, F, A] =
    enumerableT(apply compose f)

}

object EnumerableT extends EnumerableTs

trait EnumerableTs {
  type Enumerable[X, E, A] =
  EnumerableT[X, E, Identity, A]

  def enumerableT[X, E, F[_], A](a: X => EnumerateeT[E, F, A]): EnumerableT[X, E, F, A] =
    new EnumerableT[X, E, F, A] {
      val apply = a
    }

  def enumerable[X, E, A](z: X => Enumeratee[E, A]): Enumerable[X, E, A] =
    enumerable(z)

  implicit def StreamEnumerable[E, A]: Enumerable[Stream[E], E, A] = new Enumerable[Stream[E], E, A] {
    val apply: Stream[E] => Enumeratee[E, A] =
      x =>
        enumeratee((i: E >@> A) => x match {
          case Stream() => i
          case x #:: xs => i.fold(done = (_, _) => i, cont = k => apply(xs) enumerate k(elInput(x)) value)
        })
  }

  def iteratorLikeEnumerable[I, E, A](next: I => E, nextable: E => Boolean): EnumerableT[I, E, IO, A] =
    new EnumerableT[I, E, IO, A] {
      val apply =
        (x: I) => {
          def loop: Iteratee[E, A] => IterT[E, IO, A] =
            i => {
              val ii = IO(i mapI (new (Identity ~> IO) {
                         def apply[A](x: Identity[A]) = IO(x.value)
                       }))
              i.fold(
                done = (_, _) => ii
              , cont = k => for {
                  e <- IO(next(x))
                  a <- if (nextable(e)) loop(k(elInput(e))) else ii
                } yield a
              )
            }
          enumerateeT(loop)
        }
    }

  implicit def IteratorEnumerable[E, A]: EnumerableT[Iterator[E], E, IO, A] =
    iteratorLikeEnumerable(_.next, _ != null)

  implicit def ReaderEnumerable[A]: EnumerableT[Reader, IoExceptionOr[Char], IO, A] =
    iteratorLikeEnumerable(i => IoExceptionOr(i.read.toChar), _ exists (_ != -1))

  implicit def BufferedReaderEnumerable[A]: EnumerableT[BufferedReader, IoExceptionOr[String], IO, A] =
    iteratorLikeEnumerable(i => IoExceptionOr(i.readLine), _ exists (_ != null))

  implicit def InputStreamEnumerable[A]: EnumerableT[InputStream, IoExceptionOr[Byte], IO, A] =
    iteratorLikeEnumerable(i => IoExceptionOr(i.read.toByte), _ exists (_ != -1))


}
