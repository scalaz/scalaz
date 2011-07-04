package scalaz
package iteratee

import IterateeT._
import EnumerateeT._
import Input._
import effect.IO
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

  implicit def ReaderEnumerable[A]: EnumerableT[Reader, Char, IO, A] =
    new EnumerableT[Reader, Char, IO, A] {
      val apply =
        (x: Reader) => {
          def loop: Iteratee[Char, A] => IterT[Char, IO, A] =
            i => {
              val ii = IO(i mapI (new (Identity ~> IO) {
                         def apply[A](x: Identity[A]) = IO(x.value)
                       }))
              i.fold(
                done = (_, _) => ii
              , cont = k => for {
                  c <- IO(x.read)
                  a <- if (c == -1) ii else loop(k(elInput(c.toChar)))
                } yield a
              )
            }
          enumerateeT(loop)
        }
    }

  implicit def BufferedReaderEnumerable[A]: EnumerableT[BufferedReader, String, IO, A] =
    new EnumerableT[BufferedReader, String, IO, A] {
      val apply =
        (x: BufferedReader) => {
          def loop: Iteratee[String, A] => IterT[String, IO, A] =
            i => {
              val ii = IO(i mapI (new (Identity ~> IO) {
                         def apply[A](x: Identity[A]) = IO(x.value)
                       }))
              i.fold(
                done = (_, _) => ii
              , cont = k => for {
                  s <- IO(x.readLine)
                  a <- if (s == null) ii else loop(k(elInput(s)))
                } yield a
              )
            }
          enumerateeT(loop)
        }
    }

  implicit def InputStreamEnumerable[A]: EnumerableT[InputStream, Byte, IO, A] =
    new EnumerableT[InputStream, Byte, IO, A] {
      val apply =
        (x: InputStream) => {
          def loop: Iteratee[Byte, A] => IterT[Byte, IO, A] =
            i => {
              val ii = IO(i mapI (new (Identity ~> IO) {
                         def apply[A](x: Identity[A]) = IO(x.value)
                       }))
              i.fold(
                done = (_, _) => ii
              , cont = k => for {
                  c <- IO(x.read)
                  a <- if (c == -1) ii else loop(k(elInput(c.toByte)))
                } yield a
              )
            }
          enumerateeT(loop)
        }
    }

  implicit def BufferedInputStreamEnumerable[A]: EnumerableT[BufferedInputStream, Byte, IO, A] =
    new EnumerableT[BufferedInputStream, Byte, IO, A] {
      val apply =
        (x: BufferedInputStream) => {
          def loop: Iteratee[Byte, A] => IterT[Byte, IO, A] =
            i => {
              val ii = IO(i mapI (new (Identity ~> IO) {
                         def apply[A](x: Identity[A]) = IO(x.value)
                       }))
              i.fold(
                done = (_, _) => ii
              , cont = k => for {
                  c <- IO(x.read)
                  a <- if (c == -1) ii else loop(k(elInput(c.toByte)))
                } yield a
              )
            }
          enumerateeT(loop)
        }
    }

}
