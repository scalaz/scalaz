package scalaz
package concurrent

import MVar._
import effect.IO

sealed abstract class Chan[A] {
  def read: IO[A]
  def write(a: A): IO[Unit]
}

object Chan {
  private[concurrent] type ChStream[A] = MVar[ChItem[A]]

  def newChan[A]: IO[Chan[A]] =
    for {
      hole     <- newEmptyMVar[ChItem[A]]
      readVar  <- newMVar(hole)
      writeVar <- newMVar(hole)
    } yield new ChanImpl[A](readVar, writeVar)
}

import Chan._
private[this] case class ChItem[A](a: A, end: ChStream[A])
private[this] class ChanImpl[A](readVar: MVar[ChStream[A]], writeVar: MVar[ChStream[A]]) extends Chan[A] {
  def read =
    readVar.modify(readEnd =>
      for {
        item <- readEnd.read
      } yield (item.end, item.a))

  def write(a: A) =
    for {
      newHole <- newEmptyMVar[ChItem[A]]
      _       <-
        for {
          oldHole <- writeVar.take
          _       <- oldHole.put(ChItem(a, newHole))
          _       <- writeVar.put(newHole)
        } yield ()
    } yield ()
}
