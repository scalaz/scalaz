package scalaz
package iteratee

import IterateeT._
import Input._
import Identity._
import effect._
import java.io.{InputStream, Reader, BufferedReader}

sealed trait EnumeratorT[E, F[_], A] {
  val enumerateT: IterateeT[E, F, A] => IterT[E, F, A]

  def enumerate(i: Iteratee[E, A])(implicit x: IterateeT[E, Identity, A] =:= IterateeT[E, F, A], y: IterT[E, F, A] =:= IterT[E, Identity, A]): Iteratee[E, A] =
    enumerateT(i).value

  def enumerateUp[G[_]](i: IterateeT[E, G, A])(implicit p: Pointed[F], t: CoPointedFunctor[G]): IterT[E, F, A] =
    enumerateT(i.up[F])

}

object EnumeratorT extends EnumeratorTs {
  def apply[E, F[_], A](k: IterateeT[E, F, A] => IterT[E, F, A]): EnumeratorT[E, F, A] =
    enumeratorT[E, F, A](k)
}

trait EnumeratorTs {
  type Enumerator[E, A] =
  EnumeratorT[E, Identity, A]

  def enumeratorT[E, F[_], A](k: IterateeT[E, F, A] => IterT[E, F, A]): EnumeratorT[E, F, A] = new EnumeratorT[E, F, A] {
    val enumerateT = k
  }

  def enumerator[E, A](k: Iteratee[E, A] => Iteratee[E, A]): Enumerator[E, A] =
    enumeratorT[E, Identity, A](i => id(k(i)))

  implicit def StreamEnumerator[E, A](x: Stream[E]): Enumerator[E, A] =
    enumerator((i: E >@> A) => x match {
      case Stream() => i
      case x #:: xs => i.fold(done = (_, _) => i, cont = k => StreamEnumerator(xs) enumerateT k(elInput(x)) value)
    })

  implicit def ListEnumerator[E, A](x: List[E]): Enumerator[E, A] =
    enumerator((i: E >@> A) => x match {
      case Nil => i
      case x :: xs => i.fold(done = (_, _) => i, cont = k => ListEnumerator(xs) enumerateT k(elInput(x)) value)
    })

  implicit def EphemeralStreamEnumerator[E, A](x: EphemeralStream[E]): Enumerator[E, A] =
    enumerator((i: E >@> A) =>
      if (x.isEmpty) i
      else i.fold(done = (_, _) => i, cont = k => EphemeralStreamEnumerator(x.tail()) enumerateT k(elInput(x.head())) value))

  def streamingEnumerator[I, E, X, A](x: I, next: I => X, conv: X => E, nextable: (E, X) => Boolean): EnumeratorT[E, IO, A] = {
    def loop: IterateeT[E, IO, A] => IterT[E, IO, A] =
      i => {
        i.foldT(
          done = (_, _) => IO(i)
          , cont = k => {
            val z = next(x)
            val c = conv(z)
            if (nextable(c, z)) k(elInput(c)) flatMap (loop(_)) else IO(i)
          }
        )

      }
    enumeratorT(loop)
  }

  implicit def IteratorEnumerator[E, A](x: Iterator[E]): EnumeratorT[E, IO, A] = {
    def loop: IterateeT[E, IO, A] => IterT[E, IO, A] =
      i => {
        i.foldT(
          done = (_, _) => IO(i)
          , cont = k => {
            if (x.hasNext) {
              val n = x.next
              k(elInput(n)) flatMap (loop(_))
            } else IO(i)
          }
        )

      }
    enumeratorT(loop)
  }

  implicit def ReaderEnumerator[A](x: Reader): EnumeratorT[IoExceptionOr[Char], IO, A] =
    streamingEnumerator(x, (i: Reader) => IoExceptionOr(i.read), (_: IoExceptionOr[Int]) map (_.toChar), (_, n: IoExceptionOr[Int]) => n exists (_ != -1))

  implicit def BufferedReaderEnumerator[A](x: BufferedReader): EnumeratorT[IoExceptionOr[String], IO, A] =
    streamingEnumerator(x, (i: BufferedReader) => IoExceptionOr(i.readLine), (z: IoExceptionOr[String]) => z, (_, n: IoExceptionOr[String]) => n exists (_ != null))

  implicit def InputStreamEnumerator[A](x: InputStream): EnumeratorT[IoExceptionOr[Byte], IO, A] =
    streamingEnumerator(x, (i: InputStream) => IoExceptionOr(i.read), (_: IoExceptionOr[Int]) map (_.toByte), (_, n: IoExceptionOr[Int]) => n exists (_ != -1))

}
