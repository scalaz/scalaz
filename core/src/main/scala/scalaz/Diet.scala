package scalaz

import scala.annotation.tailrec
import Scalaz._

sealed trait Diet[+A] {
  def contains[B >: A](value: B)(implicit E: Enum[B]): Boolean
  def +[B >: A](value: B)(implicit E: Enum[B]): Diet[B]
  def -[B >: A](value: B)(implicit E: Enum[B]): Diet[B]
  def merge[B >: A](other: Diet[B])(implicit E: Enum[B]): Diet[B]
}

case object DietEmpty extends Diet[Nothing] {
  def contains[B >: Nothing](value: B)(implicit E: Enum[B]): Boolean = false

  def +[B >: Nothing](value: B)(implicit E: Enum[B]): Diet[B] = DietNode(value, value, DietEmpty, DietEmpty)

  def -[B >: Nothing](value: B)(implicit E: Enum[B]): Diet[B] = DietEmpty

  def merge[B >: Nothing](other: Diet[B])(implicit E: Enum[B]): Diet[B] = other
}

sealed case class DietNode[+A](start: A, end: A, left: Diet[A], right: Diet[A]) extends Diet[A] {
  def contains[B >: A](value: B)(implicit E: Enum[B]): Boolean = {
    (start <= value && value <= end) || left.contains(value) || right.contains(value)
  }

  def +[B >: A](value: B)(implicit E: Enum[B]): Diet[B] = {
    if (value < start) {
      if (value.succ == start) DietNode(value, end, left, right).joinLeft
      else DietNode(start, end, left + value, right)
    } else if (value > end) {
      if (value.pred == end) DietNode(start, value, left, right).joinRight
      else DietNode(start, end, left, right + value)
    } else this
  }

  def -[B >: A](value: B)(implicit E: Enum[B]): Diet[B] = {
    if (value < start) DietNode(start, end, left - value, right)
    else if (value > end) DietNode(start, end, left, right - value)
    else if (value == start) {
      if (start == end) left.merge[B](right)
      else DietNode((start: B).succ, end, left, right)
    } else if (value == end) DietNode(start, (end: B).pred, left, right)
    else DietNode(start, value.pred, left, DietNode(value.succ, end, DietEmpty, right))
  }

  def splitMax(): (Diet[A], (A, A)) = {
    right match {
      case DietEmpty => (left, (start, end))
      case rightAsNode@ DietNode(_, _, _, _) => {
        val (splitRight, splitRange) = rightAsNode.splitMax()
        (DietNode(start, end, left, splitRight), splitRange)
      }
    }
  }

  def splitMin(): (Diet[A], (A, A)) = {
    left match {
      case DietEmpty => (right, (start, end))
      case leftAsNode@ DietNode(_, _, _, _) => {
        val (splitLeft, splitRange) = leftAsNode.splitMin()
        (DietNode(start, end, splitLeft, right), splitRange)
      }
    }
  }

  def joinLeft[B >: A](implicit E: Enum[B]): Diet[B] = {
    left match {
      case DietEmpty => this
      case leftAsNode@ DietNode(_, _, _, _) => {
        val (splitLeft, (splitStart, splitEnd)) = leftAsNode.splitMax
        if ((splitEnd: B).succ == start) DietNode(splitStart, end, splitLeft, right)
        else this
      }
    }
  }
  def joinRight[B >: A](implicit E: Enum[B]): Diet[B]  = {
    right match {
      case DietEmpty => this
      case rightAsNode@ DietNode(_, _, _, _) => {
        val (splitRight, (splitStart, splitEnd)) = rightAsNode.splitMin
        if ((end: B).succ == splitStart) DietNode(start, splitEnd, left, splitRight)
        else this
      }
    }
  }
  
  def merge[B >: A](other: Diet[B])(implicit E: Enum[B]): Diet[B] = {
    other match {
      case DietEmpty => this
      case otherAsNode@ DietNode(_, _, _, _) => {
        val (otherLeft, (otherStart, otherEnd)) = otherAsNode.splitMax
        DietNode(start, end, otherLeft, right)
      }
    }
  }
}
