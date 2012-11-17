package scalaz

import scala.annotation.tailrec
import Scalaz._
import Free._
import scala.math

sealed trait Diev[+A] {
  def +[B >: A](cakeInterval: (B, B))(implicit E: Enum[B]): Diev[B]

  def +[B >: A](value: B)(implicit E: Enum[B]): Diev[B]

  def -[B >: A](interval: (B, B))(implicit E: Enum[B]): Diev[B]

  def -[B >: A](value: B)(implicit E: Enum[B]): Diev[B]

  def contains[B >: A](value: B)(implicit E: Enum[B]): Boolean

  def contains[B >: A](interval: (B, B))(implicit E: Enum[B]): Boolean

  def foldLeft[B >: A, C](z: C)(f: (C, B) => C)(implicit E: Enum[B]): C

  def toSet[B >: A](implicit E: Enum[B]): Set[B]
}


object Diev {
  def subtractInterval[B](minuend: (B, B), subtraend: (B, B))(implicit E: Enum[B]): Vector[(B, B)] = {
    val startOverlap = if(subtraend._1 > minuend._1) Vector((minuend._1, subtraend._1.pred)) else Vector()
    //println("startOverlap = " + startOverlap)
    val endOverlap = if(subtraend._2 < minuend._2) Vector((subtraend._2.succ, minuend._2)) else Vector()
    //println("endOverlap = " + endOverlap)
    startOverlap ++ endOverlap
  }

  def fixIntervalOrder[B](interval: (B, B))(implicit E: Enum[B]): (B, B) = if (interval._2 < interval._1) (interval._2, interval._1) else interval

  def empty[A]: Diev[A] = DieVector()

  private[this] case class DieVector[+A](intervals: Vector[(A, A)] = Vector()) extends Diev[A] {
    val liftedIntervals = intervals.lift

    private[this] sealed abstract class SearchResult
    private[this] sealed case class Coincidence(position: Int) extends SearchResult
    private[this] sealed case class Between(before: Option[Int], after: Option[Int]) extends SearchResult {
      def adjacentBefore[B >: A](interval: (B, B))(implicit E: Enum[B]): Option[Int] = before.filter{pos => (intervals(pos)._2: B).succ === interval._1}
      def adjacentAfter[B >: A](interval: (B, B))(implicit E: Enum[B]): Option[Int] = after.filter{pos => (intervals(pos)._1: B).pred === interval._2}
    }

    private def construct[B >: A](prefixCount: Int, middle: Vector[(B, B)], suffixStart: Int): Diev[B] = {
      DieVector(intervals.take(prefixCount) ++ middle ++ intervals.drop(suffixStart))
    }

    private[this] def binarySearch[B >: A](value: B)(implicit E: Enum[B]): SearchResult = {
      @tailrec
      def innerSearch(min: Int = 0, max: Int = intervals.size): SearchResult = {
        //println("innerSearch(min = %s, max = %s)".format(min, max))
        if (max <= min) {
          val adjustedPosition = 0.max(min.min(max).min(intervals.size - 1))
          //println("adjustedPosition = " + adjustedPosition)
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

    def subtractInterval[B](minuend: (B, B), subtraend: (B, B))(implicit E: Enum[B]): Vector[(B, B)] = {
      val startOverlap = if(subtraend._1 > minuend._1) Vector((minuend._1, subtraend._1.pred)) else Vector()
      //println("startOverlap = " + startOverlap)
      val endOverlap = if(subtraend._2 < minuend._2) Vector((subtraend._2.succ, minuend._2)) else Vector()
      //println("endOverlap = " + endOverlap)
      startOverlap ++ endOverlap
    }

    def fixIntervalOrder[B >: A](interval: (B, B))(implicit E: Enum[B]): (B, B) = if (interval._2 < interval._1) (interval._2, interval._1) else interval

    def +[B >: A](interval: (B, B))(implicit E: Enum[B]): Diev[B] = {
      (binarySearch(interval._1), binarySearch(interval._2)) match {
        case (Coincidence(startPosition), Coincidence(endPosition)) => {
          construct(startPosition, Vector((intervals(startPosition)._1.min(interval._1), intervals(endPosition)._2.max(interval._2))), endPosition + 1)
        }
        case (Coincidence(startPosition), between@Between(_, after)) => {
          val adjustedAfter = between.adjacentAfter(interval).orElse(after)
          construct(startPosition, Vector((intervals(startPosition)._1.min(interval._1), adjustedAfter.map(intervals(_)._2).getOrElse(interval._2))), adjustedAfter.getOrElse(intervals.size))
        }
        case (earlyBound@ Between(before, after), Coincidence(endPosition)) => {
          val adjustedBefore = earlyBound.adjacentBefore(interval).orElse(before)
          construct(adjustedBefore.getOrElse(0), Vector((adjustedBefore.map(intervals(_)._1).getOrElse(interval._1), intervals(endPosition)._2.max(interval._2))), endPosition + 1)
        }
        case (earlyBound@ Between(before, after), lateBound@Between(_, otherAfter)) => {
          val adjacentBeforeResult = earlyBound.adjacentBefore(interval)
          val adjacentAfterResult = lateBound.adjacentAfter(interval)
          construct(
            adjacentBeforeResult.orElse(before.map(_ + 1)).getOrElse(0),
            Vector((adjacentBeforeResult.map(intervals(_)._1).getOrElse(interval._1), adjacentAfterResult.map(intervals(_)._2).getOrElse(interval._2))),
            adjacentAfterResult.map(_ + 1).orElse(after).getOrElse(intervals.size)
          )
        }
      }
    }

    def +[B >: A](value: B)(implicit E: Enum[B]): Diev[B] = this + (value, value)

    def -[B >: A](interval: (B, B))(implicit E: Enum[B]): Diev[B] = {
      val orderedInterval = fixIntervalOrder(interval)
      (binarySearch(orderedInterval._1), binarySearch(orderedInterval._2)) match {
        case (Coincidence(startPosition), Coincidence(endPosition)) => {
          val middle = if (startPosition == endPosition) subtractInterval(intervals(startPosition), interval)
          else subtractInterval(intervals(startPosition), interval) ++ subtractInterval(intervals(endPosition), interval)
          construct(startPosition, middle, endPosition + 1)
        }
        case (Coincidence(startPosition), Between(_, endAfter)) => {
          val middle = subtractInterval(intervals(startPosition), interval)
          construct(startPosition, middle, endAfter.map(endAfterPos => endAfterPos + 1).getOrElse(intervals.size))
        }
        case (Between(startBefore, _), Coincidence(endPosition)) => {
          val middle = subtractInterval(intervals(endPosition), interval)
          construct(startBefore.map(startBeforePos => startBeforePos + 1).orZero, middle, endPosition + 1)
        }
        case (Between(startBefore, _), Between(_, endAfter)) => {
          construct(startBefore.map(startBeforePos => startBeforePos + 1).orZero, Vector.empty, endAfter.map(endAfterPos => endAfterPos + 1).orZero)
        }
      }
    }

    def -[B >: A](value: B)(implicit E: Enum[B]): Diev[B] = this - (value, value)

    def contains[B >: A](value: B)(implicit E: Enum[B]): Boolean = binarySearch(value) match {
      case Coincidence(_) => true
      case _ => false
    }

    def contains[B >: A](interval: (B, B))(implicit E: Enum[B]): Boolean = binarySearch(interval._1) match {
      case Coincidence(position) if (intervals(position)._2 >= interval._2) => true
      case _ => false
    }

    def foldLeft[B >: A, C](z: C)(f: (C, B) => C)(implicit E: Enum[B]): C = {
      intervals.foldLeft(z){(z1, interval) =>
        val range = (interval._1: B) |-> (interval._2: B)
        range.foldLeft(z1)(f)
      }
    }

    def toSet[B >: A](implicit E: Enum[B]): Set[B] = foldLeft[B, Set[B]](Set[B]())(_ + _)

    override def toString(): String = intervals.view.map(interval => "(%s -> %s)".format(interval._1, interval._2)).mkString(", ")
  }
}
