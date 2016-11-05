package scalaz

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import syntax.enum._

/**
 * Implementation of a Discrete Interval Encoding Tree [[http://web.engr.oregonstate.edu/~erwig/diet/]] that
 * is actually implemented using a Vector and is balanced at all times as a result.
 */
sealed abstract class Diev[A] {
  def +(interval: (A, A)): Diev[A]

  def +(value: A): Diev[A]

  def -(interval: (A, A)): Diev[A]

  def -(value: A): Diev[A]

  def ++(other: Diev[A]): Diev[A]

  def --(other: Diev[A]): Diev[A]

  def intervals: Vector[(A, A)]

  def contains(value: A): Boolean

  def contains(interval: (A, A)): Boolean

  def map[B](f: A => B)(implicit EB: Enum[B]): Diev[B]

  def flatMap[B](f: A => Diev[B])(implicit EB: Enum[B]): Diev[B]

  def filter(f: A => Boolean): Diev[A]

  def foreach(f: A => Unit): Unit

  def foldLeft[B](z: B)(f: (B, A) => B): B

  def toSet(): Set[A]

  def toList(): List[A]
}

object DievInterval {
  def subtractInterval[A](minuend: (A, A), subtraend: (A, A))(implicit E: Enum[A]): Vector[(A, A)] = {
    val startOverlap = if(subtraend._1 > minuend._1) Vector((minuend._1, subtraend._1.pred)) else Vector()
    //println("startOverlap = " + startOverlap)
    val endOverlap = if(subtraend._2 < minuend._2) Vector((subtraend._2.succ, minuend._2)) else Vector()
    //println("endOverlap = " + endOverlap)
    startOverlap ++ endOverlap
  }

  def fixIntervalOrder[A](interval: (A, A))(implicit E: Enum[A]): (A, A) = if (interval._2 < interval._1) interval.swap else interval
}

trait DievImplementation {
  import syntax.std.option._
  import std.anyVal._
  import DievInterval._
  protected[this] case class DieVector[A](intervals: Vector[(A, A)] = Vector())(implicit EA: Enum[A]) extends Diev[A] {
    val liftedIntervals = intervals.lift

    private[this] sealed abstract class SearchResult
    private[this] sealed case class Coincidence(position: Int) extends SearchResult
    private[this] sealed case class Between(before: Option[Int], after: Option[Int]) extends SearchResult {
      def adjacentBefore(interval: (A, A)): Option[Int] = before.filter{pos => intervals(pos)._2.succ === interval._1}
      def adjacentAfter(interval: (A, A)): Option[Int] = after.filter{pos => intervals(pos)._1.pred === interval._2}
    }

    private def construct(prefixCount: Int, middle: Vector[(A, A)], suffixStart: Int): Diev[A] = {
      DieVector(intervals.take(prefixCount) ++ middle ++ intervals.drop(suffixStart))
    }

    private[this] def binarySearch(value: A): SearchResult = {
      @tailrec
      def innerSearch(min: Int = 0, max: Int = intervals.size): SearchResult = {
        if (max <= min) {
          val adjustedPosition = 0.max(min.min(max).min(intervals.size - 1))
          liftedIntervals(adjustedPosition) match {
            case Some((start, end)) => {
              if (start <= value && value <= end) Coincidence(adjustedPosition)
              else {
                if (value < start) Between(liftedIntervals(adjustedPosition - 1).map(_ => adjustedPosition - 1), adjustedPosition.some)
                else Between(adjustedPosition.some, liftedIntervals(adjustedPosition + 1).map(_ => adjustedPosition + 1))
              }
            }
            case _ => Between(None, None)
          }
        } else {
          val mid = min + ((max - min) / 2)

          intervals(mid) match {
            case (start, end) => {
              if (start <= value && value <= end) Coincidence(mid)
              else {
                if (value < start) innerSearch(min, mid - 1)
                else innerSearch(mid + 1, max)
              }
            }
          }
        }
      }

      val resultOfSearch = innerSearch()
      //println("resultOfSearch = " + resultOfSearch)
      resultOfSearch
    }

    def +(interval: (A, A)): Diev[A] = {
      val correctedInterval = fixIntervalOrder(interval)
      (binarySearch(correctedInterval._1), binarySearch(correctedInterval._2)) match {
        case (Coincidence(startPosition), Coincidence(endPosition)) => {
          construct(startPosition, Vector((intervals(startPosition)._1.min(correctedInterval._1), intervals(endPosition)._2.max(correctedInterval._2))), endPosition + 1)
        }
        case (Coincidence(startPosition), between@Between(_, after)) => {
          val adjacentAfterResult = between.adjacentAfter(correctedInterval)
          construct(
            startPosition,
            Vector((intervals(startPosition)._1.min(correctedInterval._1), adjacentAfterResult.map(intervals(_)._2).getOrElse(correctedInterval._2))),
            adjacentAfterResult.map(_ + 1).orElse(after).getOrElse(intervals.size)
          )
        }
        case (earlyBound@ Between(before, after), Coincidence(endPosition)) => {
          val adjacentBeforeResult = earlyBound.adjacentBefore(correctedInterval)
          construct(
            adjacentBeforeResult.orElse(before.map(_ + 1)).getOrElse(0),
            Vector((adjacentBeforeResult.map(intervals(_)._1).getOrElse(correctedInterval._1), intervals(endPosition)._2.max(correctedInterval._2))),
            endPosition + 1
          )
        }
        //(Between(None,Some(0)),Between(Some(0),Some(1)))
        case (earlyBound@ Between(before, after), lateBound@Between(_, otherAfter)) => {
          val adjacentBeforeResult = earlyBound.adjacentBefore(correctedInterval)
          val adjacentAfterResult = lateBound.adjacentAfter(correctedInterval)
          construct(
            adjacentBeforeResult.orElse(before.map(_ + 1)).getOrElse(0),
            Vector((adjacentBeforeResult.map(intervals(_)._1).getOrElse(correctedInterval._1), adjacentAfterResult.map(intervals(_)._2).getOrElse(correctedInterval._2))),
            adjacentAfterResult.map(_ + 1).orElse(otherAfter).getOrElse(intervals.size)
          )
        }
      }
    }

    def +(value: A): Diev[A] = this + (value, value)

    def -(interval: (A, A)): Diev[A] = {
      val orderedInterval = fixIntervalOrder(interval)
      (binarySearch(orderedInterval._1), binarySearch(orderedInterval._2)) match {
        case (Coincidence(startPosition), Coincidence(endPosition)) => {
          val middle = if (startPosition == endPosition) subtractInterval(intervals(startPosition), interval)
          else subtractInterval(intervals(startPosition), interval) ++ subtractInterval(intervals(endPosition), interval)
          construct(startPosition, middle, endPosition + 1)
        }
        case (Coincidence(startPosition), Between(_, endAfter)) => {
          val middle = subtractInterval(intervals(startPosition), orderedInterval)
          construct(startPosition, middle, endAfter.getOrElse(intervals.size))
        }
        case (Between(startBefore, _), Coincidence(endPosition)) => {
          val middle = subtractInterval(intervals(endPosition), orderedInterval)
          construct(startBefore.map(startBeforePos => startBeforePos + 1).orZero, middle, endPosition + 1)
        }
        case (Between(startBefore, _), Between(_, endAfter)) => {
          construct(startBefore.map(startBeforePos => startBeforePos + 1).orZero, Vector.empty, endAfter.getOrElse(intervals.size))
        }
      }
    }

    def -(value: A): Diev[A] = this - (value, value)

    def ++(other: Diev[A]): Diev[A] = other.intervals.foldLeft(this: Diev[A])(_ + _)

    def --(other: Diev[A]): Diev[A] = other.intervals.foldLeft(this: Diev[A])(_ - _)

    def contains(value: A): Boolean = binarySearch(value) match {
      case Coincidence(_) => true
      case _ => false
    }

    def contains(interval: (A, A)): Boolean = binarySearch(interval._1) match {
      case Coincidence(position) if (intervals(position)._2 >= interval._2) => true
      case _ => false
    }

    def map[B](f: A => B)(implicit EB: Enum[B]): Diev[B] = foldLeft[Diev[B]](DieVector[B]())(_ + f(_))

    def flatMap[B](f: A => Diev[B])(implicit EB: Enum[B]): Diev[B] = foldLeft[Diev[B]](DieVector[B]())(_ ++ f(_))

    def filter(f: A => Boolean): Diev[A] = foldLeft[Diev[A]](DieVector[A]())((working, value) => if (f(value)) working + value else working)

    def foreach(f: A => Unit): Unit = foldLeft[Unit](())((_, value) => f(value))

    def foldLeft[B](z: B)(f: (B, A) => B): B = {
      intervals.foldLeft(z){(z1, interval) =>
        val range = interval._1 |-> interval._2
        range.foldLeft(z1)(f)
      }
    }

    def toSet(): Set[A] = foldLeft[Set[A]](Set[A]())(_ + _)

    def toList(): List[A] = foldLeft[ListBuffer[A]](new ListBuffer())(_ += _).toList

    override def toString(): String = intervals.foldLeft(new StringBuilder().append("("))(_.append(_)).append(")").toString
  }
}

object Diev extends DievInstances {
  def empty[A](implicit E: Enum[A]): Diev[A] = DieVector()

  def fromValuesSeq[A](values: Seq[A])(implicit E: Enum[A]): Diev[A] = values.foldLeft(empty[A])(_ + _)

  def fromIntervalsSeq[A](intervals: Seq[(A, A)])(implicit E: Enum[A]): Diev[A] = intervals.foldLeft(empty[A])(_ + _)
}

sealed abstract class DievInstances extends DievImplementation {
  import std.tuple._, std.vector._

  implicit def dievEqual[A: Equal]: Equal[Diev[A]] = Equal.equalBy[Diev[A], Vector[(A, A)]](_.intervals)(std.vector.vectorEqual[(A, A)])

  implicit def dievMonoid[A: Enum]: Monoid[Diev[A]] = new Monoid[Diev[A]] {
    def append(f1: Diev[A], f2: => Diev[A]) = f1 ++ f2

    def zero: Diev[A] = new DieVector[A]()
  }

  implicit def dievShow[A: Show]: Show[Diev[A]] = new Show[Diev[A]] {
    override def show(diev: Diev[A]) = Show[Vector[(A, A)]].show(diev.intervals)
  }
}
