// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz.effect

import scalaz.data.Maybe
import scalaz.data.Disjunction._

import java.util.concurrent.atomic.AtomicReference

import MVarInternal._

private[effect] final class MVarImpl[A](threadPool: (=> Unit) => Unit, val state: AtomicReference[MVarState[A]]) extends MVar[A] {
  final def peek: IO[Maybe[A]] = IO.sync {
    state.get match {
      case Surplus(head, _) => Maybe.just(head)
      case Deficit(_, _) => Maybe.empty[A]
    }
  }

  final def take: IO[A] = IO.async0 { taker =>
    var loop  = true
    var finish: Action[A] = null

    while (loop) {
      finish = null

      val oldState = state.get

      val newState = oldState match {
        case Surplus(value0, putters) =>
          if (putters.length == 0) {
            finish = () => AsyncReturn.now(value0)

            Empty
          } else {
            val (value, putter0) = putters(0)

            finish = () => {
              threadPool(putter0(SuccessUnit))

              AsyncReturn.now(value0)
            }

            Surplus(value, putters.tail)
          }
        case Deficit(takers, readers) =>
          finish = () => AsyncReturn.maybeLater(removeTaker(taker, _))

          Deficit(takers :+ taker, readers)
      }

      if (state.compareAndSet(oldState, newState)) loop = false
    }

    finish()
  }

  final def read: IO[A] = IO.async0 { reader =>
    var loop = true
    var finish: Action[A] = null

    while (loop) {
      val oldState = state.get

      val newState = oldState match {
        case Surplus(v, _) =>
          finish = () => AsyncReturn.now(v)

          oldState

        case Deficit(takers, readers) =>
          finish = () => AsyncReturn.maybeLater(removeReader(reader, _))

          Deficit(takers, readers :+ reader)
      }

      if (state.compareAndSet(oldState, newState)) loop = false
    }

    finish()
  }

  final def put(v: A): IO[Unit] = IO.async0 { putter =>
    var loop = true
    var finish: Action[Unit] = null

    while (loop) {
      val oldState = state.get

      val newState = oldState match {
        case Surplus(v2, putters) =>
          finish = () => AsyncReturn.maybeLater(removePutter(putter, _))

          Surplus(v2, putters :+ ((v, putter)))

        case Deficit(takers, readers) =>
          val (newState, finish2) = doPut(v, readers, takers)

          finish = finish2

          newState
      }

      if (state.compareAndSet(oldState, newState)) loop = false
    }

    finish()
  }

  final def tryPut(v: A): IO[Boolean] = IO.sync {
    var loop = true
    var value = false
    var finish = UnitAction

    while (loop) {
      finish = UnitAction

      val oldState = state.get

      val newState = oldState match {
        case Surplus(_, _) =>
          value = false

          oldState

        case Deficit(takers, readers) =>
          val (newState, finish2) = doPut(v, readers, takers)

          finish = finish2
          value  = true

          newState
      }

      if (state.compareAndSet(oldState, newState)) loop = false
    }

    finish()

    value
  }

  final def tryTake: IO[Maybe[A]] = IO.sync {
    var loop = true
    var finish: () => Unit = null
    var value: Maybe[A] = null.asInstanceOf[Maybe[A]]

    while (loop) {
      val oldState = state.get

      val newState = oldState match {
        case Deficit(_, _) =>
          value = Maybe.empty[A]

          oldState

        case Surplus(value0, putters) =>
          value = Maybe.just(value0)

          if (putters.length == 0) Deficit(Vector(), Vector())
          else {
            val (value, putter0) = putters(0)

            finish = () => threadPool(putter0(SuccessUnit))

            Surplus(value, putters.tail)
          }
      }

      if (state.compareAndSet(oldState, newState)) loop = false
    }

    if (finish != null) finish()

    value
  }

  private final def doPut(v: A, readers: Vector[Callback[A]], takers: Vector[Callback[A]]): (MVarState[A], Action[Unit]) = {
    val result: Try[A] = \/-(v)

    if (takers.length == 0) (Surplus(v, Vector()), UnitAction)
    else {
      val taker = takers(0)

      (if (takers.length == 1) Empty[A] else Deficit(takers.tail, Vector()),
       () => {
         readers.foreach(reader => reader(result))
         taker(result)
         AsyncReturn.later[Unit]
       })
    }
  }

  private final def removePutter(putter: Callback[Unit], t: Throwable): Unit = {
    var loop = true
    var removed = false

    while (loop) {
      removed = false

      val oldState = state.get

      val newState = oldState match {
        case Surplus(head, tail) =>
          removed = true

          Surplus(head, tail.filter(_._2 eq putter))
        case Deficit(_, _) => oldState
      }

      if (state.compareAndSet(oldState, newState)) loop = false
    }

    if (removed) threadPool(putter(-\/(t)))
  }

  private final def removeTaker(taker: Callback[A], t: Throwable): Unit = {
    var loop = true
    var removed = false

    while (loop) {
      removed = false

      val oldState = state.get

      val newState = oldState match {
        case Surplus(_, _) => oldState
        case Deficit(takers, readers) =>
          removed = true

          Deficit(takers.filter(_ eq taker), readers)
      }

      if (state.compareAndSet(oldState, newState)) loop = false
    }

    if (removed) threadPool(taker(-\/(t)))
  }

  private final def removeReader(reader: Callback[A], t: Throwable): Unit = {
    var loop = true
    var removed = false

    while (loop) {
      removed = false

      val oldState = state.get

      val newState = oldState match {
        case Surplus(_, _) => oldState
        case Deficit(takers, readers) =>
          removed = true

          Deficit(takers, readers.filter(_ eq reader))
      }

      if (state.compareAndSet(oldState, newState)) loop = false
    }

    if (removed) threadPool(reader(-\/(t)))
  }
}

private[effect] object MVarInternal {
  val SuccessUnit: Try[Unit] = \/-(())

  type Action[A] = () => AsyncReturn[A]

  private val NowUnit: AsyncReturn[Unit] = AsyncReturn.now(())

  final val UnitAction: Action[Unit] = () => NowUnit

  final val CallbackUnit: Callback[Unit] = (t: Try[Unit]) => ()

  type Callback[Z] = Try[Z] => Unit

  type Try[A] = Throwable \/ A

  sealed abstract class MVarState[+A]

  final case class Surplus[A](head: A, tail: Vector[(A, Callback[Unit])]) extends MVarState[A]
  final case class Deficit[A](takers: Vector[Callback[A]], readers: Vector[Callback[A]]) extends MVarState[A]

  def Empty[A] = Deficit[A](Vector(), Vector())
}
