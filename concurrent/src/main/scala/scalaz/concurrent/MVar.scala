package scalaz
package concurrent

import Scalaz._
import effect._

import Atomic._
import PhasedLatch._

sealed abstract class MVar[A] {
  def put(a: => A): IO[Unit]
  def take: IO[A]

  final def read: IO[A] =
    for {
      a <- take
      _ <- put(a)
    } yield a

  final def modify[B](f: A => IO[(A, B)]): IO[B] =
    for {
      a <- take
      r <- f(a) onException put(a)
      _ <- put(r._1)
    }  yield r._2
}

object MVar extends MVarFunctions

trait MVarFunctions {
  def newMVar[A](a: A): IO[MVar[A]] =
    for {
      mvar <- newEmptyMVar[A]
      _    <- mvar.put(a)
    } yield mvar

  def newEmptyMVar[A]: IO[MVar[A]] =
    for {
      value <- newAtomic(none[A])
      readLatch <- newPhasedLatch
      writeLatch <- newPhasedLatch
    } yield new MVarImpl[A](value, readLatch, writeLatch)
}

private[this] class MVarImpl[A](value: Atomic[Option[A]], readLatch: PhasedLatch, writeLatch: PhasedLatch) extends MVar[A] {
  def take = read(
    for {
      a <- value.getAndSet(None)
      _ <- writeLatch.release()
    } yield a
  )

  def put(a: => A) = write(a, value.get)

  def read(reader: => IO[Option[A]]) = {
    def read_ : IO[A] =
      for {
        p <- readLatch.currentPhase
        r <- reader
        a <- r match {
          case Some(a) => IO(a)
          case None =>
            for {
              _ <- readLatch.awaitPhase(p) // we don't have a value so we wait for someone to put one
              a <- read_    // someone has put a value so now we try to read it
            } yield a
        }
      } yield a
    read_
  }

  def write(a: => A, read: => IO[Option[A]]): IO[Unit] = writeLatch.currentPhase flatMap { p =>
    read flatMap(v => v match {
      case Some(_) =>
        for {
          _ <- writeLatch awaitPhase p // if there is a value, wait until someone takes it
          _ <- write(a, read)           // someone has taken the value, try and write it again
        } yield ()
      case None =>
        value.compareAndSet(v, Some(a)) flatMap { set => // There is no value, so it's time to try and write one.
          if (!set) write(a, read)  // If the value has changed, the write will fail so we'll need to try it again.
          else readLatch.release  // If the write succeeded, release a thread waiting for a value.
        }
    })
  }
}
