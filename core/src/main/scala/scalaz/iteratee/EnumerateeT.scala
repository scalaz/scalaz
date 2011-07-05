package scalaz
package iteratee

import IterateeT._
import Input._
import Identity._
import effect._
import java.io.{InputStream, Reader, BufferedReader}

sealed trait EnumerateeT[E, F[_], A] {
  val enumerateT: IterateeT[E, F, A] => IterT[E, F, A]

  def enumerate(i: Iteratee[E, A])(implicit x: IterateeT[E, Identity, A] =:= IterateeT[E, F, A], y: IterT[E, F, A] =:= IterT[E, Identity, A]): Iteratee[E, A] =
    enumerateT(i).value

  def enumerateUp[G[_]](i: IterateeT[E, G, A])(implicit p: Pointed[F], t: CoPointedFunctor[G]): IterT[E, F, A] =
    enumerateT(i.up[F])

}

object EnumerateeT extends EnumerateeTs {
  def apply[E, F[_], A](k: IterateeT[E, F, A] => IterT[E, F, A]): EnumerateeT[E, F, A] =
    enumerateeT[E, F, A](k)
}

trait EnumerateeTs {
  type Enumeratee[E, A] =
  EnumerateeT[E, Identity, A]

  def enumerateeT[E, F[_], A](k: IterateeT[E, F, A] => IterT[E, F, A]): EnumerateeT[E, F, A] = new EnumerateeT[E, F, A] {
    val enumerateT = k
  }

  def enumeratee[E, A](k: Iteratee[E, A] => Iteratee[E, A]): Enumeratee[E, A] =
    enumerateeT[E, Identity, A](i => id(k(i)))

  implicit def StreamEnumeratee[E, A](x: Stream[E]): Enumeratee[E, A] =
    enumeratee((i: E >@> A) => x match {
        case Stream() => i
        case x #:: xs => i.fold(done = (_, _) => i, cont = k => StreamEnumeratee(xs) enumerateT k(elInput(x)) value)
      })

  implicit def ListEnumeratee[E, A](x: List[E]): Enumeratee[E, A] =
    enumeratee((i: E >@> A) => x match {
        case Nil => i
        case x :: xs => i.fold(done = (_, _) => i, cont = k => ListEnumeratee(xs) enumerateT k(elInput(x)) value)
      })

  implicit def EphemeralStreamEnumeratee[E, A](x: EphemeralStream[E]): Enumeratee[E, A] =
    enumeratee((i: E >@> A) =>
        if(x.isEmpty) i
        else i.fold(done = (_, _) => i, cont = k => EphemeralStreamEnumeratee(x.tail()) enumerateT k(elInput(x.head())) value))

  def streamingEnumeratee[I, E, X, A](x: I, next: I => X, conv: X => E, nextable: (E, X) => Boolean): EnumerateeT[E, IO, A] = {
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
    enumerateeT(loop)
  }

  implicit def IteratorEnumeratee[E, A](x: Iterator[E]): EnumerateeT[E, IO, A] = {
    def loop: IterateeT[E, IO, A] => IterT[E, IO, A] =
      i => {
        i.foldT(
          done = (_, _) => IO(i)
        , cont = k => {
            if(x.hasNext) {
              val n = x.next
              k(elInput(n)) flatMap (loop(_))
            } else IO(i)
          }
        )

      }
    enumerateeT(loop)
  }

  implicit def ReaderEnumeratee[A](x: Reader): EnumerateeT[IoExceptionOr[Char], IO, A] =
    streamingEnumeratee(x, (i: Reader) => IoExceptionOr(i.read), (_: IoExceptionOr[Int]) map (_.toChar), (_, n: IoExceptionOr[Int]) => n exists (_ != -1))
                                                                                  /*
  implicit def BufferedReaderEnumeratee[A](x: BufferedReader): EnumerateeT[IoExceptionOr[String], IO, A] =
    streamingEnumeratee(x, (i: BufferedReader) => IoExceptionOr(i.readLine), _ exists (_ != null))

  implicit def InputStreamEnumeratee[A](x: InputStream): EnumerateeT[IoExceptionOr[Byte], IO, A] =
    streamingEnumeratee(x, (i: InputStream) => IoExceptionOr(i.read.toByte), _ exists (_ != -1))
    */

}
