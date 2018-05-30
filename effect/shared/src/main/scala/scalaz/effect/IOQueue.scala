// Copyright (C) 2018 John A. De Goes. All rights reserved.

package scalaz
package effect

import scalaz.effect.IOQueue.internal._

import scala.collection.immutable.Queue

/**
 * An `IOQueue` is a lightweight, asynchronous queue. This implementation is
 * naive, if functional, and could benefit from significant optimization.
 *
 * TODO:
 *
 * 1. Investigate using a faster option than `Queue`, because `Queue` has
 *    `O(n)` `length` method.
 * 2. We are using `Promise.unsafeMake`. Why? What would the safe way of doing
 *    this be?
 * 3. Benchmark to see how slow this implementation is and if there are any
 *    easy ways to improve performance.
 * 4. There is a gap between `modifyFold` and the `Promise.get`, which means if
 *    the `take` or `offer` are interrupted between the gap, then the `ensuring`
 *    won't run, so the taker/putter won't be removed from the queue, leading to
 *    a leak of resources. This must be fixed.
 */
class IOQueue[A] private (capacity: Int, ref: IORef[State[A]]) {

  /**
   * Retrieves the size of the queue, which is equal to the number of elements
   * in the queue. This may be negative if fibers are suspended waiting for
   * elements to be added to the queue.
   */
  final def size[E]: IO[E, Int] = ref.read.map(_.size)

  /**
   * Places the value in the queue. If the queue has reached capacity, then
   * the fiber performing the `offer` will be suspended until there is room in
   * the queue.
   */
  final def offer[E](a: A): IO[E, Unit] =
    IO.flatten(ref.modifyFold[E, IO[E, Unit]] {
      case Deficit(takers) =>
        takers.dequeueOption match {
          case None                  => (IO.unit[E], Surplus(Queue.empty[A].enqueue(a), Queue.empty))
          case Some((taker, takers)) => (taker.complete[E](a).toUnit, Deficit(takers))
        }
      case Surplus(values, putters) =>
        if (values.length < capacity && putters.isEmpty) {
          (IO.unit[E], Surplus(values.enqueue(a), putters))
        } else {
          val p = Promise.unsafeMake[E, Unit]
          (p.get.ensuring(removePutter(p)), Surplus(values, putters.enqueue((a, p))))
        }
    })

  /**
   * Removes the oldest value in the queue. If the queue is empty, this will
   * return a computation that resumes when an item has been added to the queue.
   */
  final def take[E]: IO[E, A] =
    IO.flatten(ref.modifyFold[E, IO[E, A]] {
      case Deficit(takers) =>
        val p = Promise.unsafeMake[E, A]
        (p.get.ensuring(removeTaker(p)), Deficit(takers.enqueue(p)))
      case Surplus(values, putters) =>
        values.dequeueOption match {
          case None =>
            putters.dequeueOption match {
              case None =>
                val p = Promise.unsafeMake[E, A]
                (p.get.ensuring(removeTaker(p)), Deficit(Queue.empty.enqueue(p)))
              case Some(((a, putter), putters)) =>
                (putter.complete(()) *> IO.now(a), Surplus(Queue.empty, putters))
            }
          case Some((a, values)) =>
            (IO.now(a), Surplus(values, putters))
        }
    })

  /**
   * Interrupts any fibers that are suspended on `take` because the queue is
   * empty. If any fibers are interrupted, returns true, otherwise, returns
   * false.
   */
  final def interruptTake[E](t: Throwable): IO[E, Boolean] =
    IO.flatten(ref.modifyFold[E, IO[E, Boolean]] {
      case Deficit(takers) if takers.nonEmpty =>
        val forked: IO[E, Unit] = IO.forkAll(takers.toList.map(_.interrupt[E](t).toUnit))
        (forked.const(true), Deficit(Queue.empty[Promise[_, A]]))
      case s =>
        (IO.now(false), s)
    })

  /**
   * Interrupts any fibers that are suspended on `offer` because the queue is
   * at capacity. If any fibers are interrupted, returns true, otherwise,
   * returns  false.
   */
  final def interruptOffer[E](t: Throwable): IO[E, Boolean] =
    IO.flatten(ref.modifyFold[E, IO[E, Boolean]] {
      case Surplus(_, putters) if putters.nonEmpty =>
        val forked: IO[E, Unit] = IO.forkAll(putters.toList.map(_._2.interrupt[E](t).toUnit))
        (forked.const(true), Deficit(Queue.empty[Promise[_, A]]))
      case s =>
        (IO.now(false), s)
    })

  private final def removePutter(putter: Promise[_, Unit]): IO[Void, Unit] =
    ref
      .modify[Void] {
        case Surplus(values, putters) =>
          Surplus(values, putters.filterNot(_._2 == putter))
        case d => d
      }
      .toUnit

  private final def removeTaker(taker: Promise[_, A]): IO[Void, Unit] =
    ref
      .modify[Void] {
        case Deficit(takers) =>
          Deficit(takers.filterNot(_ == taker))

        case d => d
      }
      .toUnit

}
object IOQueue {

  /**
   * Makes a new queue.
   */
  final def make[E, A](capacity: Int): IO[E, IOQueue[A]] =
    IORef[E, State[A]](Surplus[A](Queue.empty, Queue.empty)).map(new IOQueue[A](capacity, _))

  private[effect] object internal {
    sealed trait State[A] {
      def size: Int
    }
    final case class Deficit[A](takers: Queue[Promise[_, A]]) extends State[A] {
      def size: Int = -takers.length
    }
    final case class Surplus[A](queue: Queue[A], putters: Queue[(A, Promise[_, Unit])]) extends State[A] {
      def size: Int = queue.size + putters.length // TODO: O(n) for putters.length
    }
  }
}
