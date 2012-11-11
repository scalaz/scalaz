package scalaz

import scala.annotation.tailrec
import Scalaz._
import Free._

sealed trait Diet[+A] {
  val depth: Int
  def contains[B >: A](value: B)(implicit E: Enum[B]): Boolean
  def +[B >: A](value: B)(implicit E: Enum[B]): Diet[B]
  def freeAdd[B >: A, S[+_]](value: B)(implicit E: Enum[B], S: Pointed[S]): Free[S, Diet[B]]
  def -[B >: A](value: B)(implicit E: Enum[B]): Diet[B]
  def freeSubtract[B >: A, S[+_]](value: B)(implicit E: Enum[B], S: Pointed[S]): Free[S, Diet[B]]
  def merge[B >: A](other: Diet[B])(implicit E: Enum[B]): Diet[B]
  def foldLeft[B](b: B)(f: (B, (A, A)) => B): B = freeFoldLeft[B, Function0](b)(f).go(_.copoint)
  def freeFoldLeft[B, S[+_]](b: B)(f: (B, (A, A)) => B)(implicit S: Pointed[S]): Free[S, B]
  override def toString(): String = foldLeft(new StringBuilder)(_.append(_)).toString
}

final case object DietEmpty extends Diet[Nothing] {
  val depth: Int = 0

  def contains[B >: Nothing](value: B)(implicit E: Enum[B]): Boolean = false

  def +[B >: Nothing](value: B)(implicit E: Enum[B]): Diet[B] = DietNode(value, value, DietEmpty, DietEmpty)

  def freeAdd[B >: Nothing, S[+_]](value: B)(implicit E: Enum[B], S: Pointed[S]): Free[S, Diet[B]] = return_(this.+(value))

  def -[B >: Nothing](value: B)(implicit E: Enum[B]): Diet[B] = DietEmpty

  def freeSubtract[B >: Nothing, S[+_]](value: B)(implicit E: Enum[B], S: Pointed[S]): Free[S, Diet[B]] = return_(this.-(value))

  def merge[B >: Nothing](other: Diet[B])(implicit E: Enum[B]): Diet[B] = other

  def freeFoldLeft[B, S[+_]](b: B)(f: (B, (Nothing, Nothing)) => B)(implicit S: Pointed[S]): Free[S, B] = return_(b)
}

sealed case class DietNode[+A](start: A, end: A, left: Diet[A], right: Diet[A]) extends Diet[A] {
  val depth = left.depth.max(right.depth) + 1

  def contains[B >: A](value: B)(implicit E: Enum[B]): Boolean = {
    (start <= value && value <= end) || left.contains(value) || right.contains(value)
  }

  def +[B >: A](value: B)(implicit E: Enum[B]): Diet[B] = freeAdd[B, Function0](value).go(_.copoint)

  def freeAdd[B >: A, S[+_]](value: B)(implicit E: Enum[B], S: Pointed[S]): Free[S, Diet[B]] = {
    if (value < start) {
      if (value.succ == start) DietNode(value, end, left, right).joinLeft
      else for {
        addedLeft <- suspend(left.freeAdd[B, S](value))
      } yield DietNode(start, end, addedLeft, right)
    } else if (value > end) {
      if (value.pred == end) DietNode(start, value, left, right).joinRight
      else for {
        addedRight <- suspend(right.freeAdd[B, S](value))
      } yield DietNode(start, end, left, addedRight)
    } else return_(this)
  }

  def -[B >: A](value: B)(implicit E: Enum[B]): Diet[B] = freeSubtract[B, Function0](value).go(_.copoint)

  def freeSubtract[B >: A, S[+_]](value: B)(implicit E: Enum[B], S: Pointed[S]): Free[S, Diet[B]] = {
    if (value < start) for {
      subtractedLeft <- left.freeSubtract[B, S](value)
    } yield DietNode(start, end, subtractedLeft, right)
    else if (value > end) for {
      subtractedRight <- right.freeSubtract[B, S](value)
    } yield DietNode(start, end, left, subtractedRight)
    else if (value == start) {
      if (start == end) return_(left.merge[B](right))
      else return_(DietNode((start: B).succ, end, left, right))
    } else if (value == end) return_(DietNode(start, (end: B).pred, left, right))
    else return_(DietNode(start, value.pred, left, DietNode(value.succ, end, DietEmpty, right)))
  }

  private[scalaz] def splitMax[B >: A, S[+_]](implicit E: Enum[B], S: Pointed[S]): Free[S, (Diet[B], (B, B))] = {
    right match {
      case DietEmpty => return_((left, (start, end)))
      case rightAsNode@ DietNode(_, _, _, _) => suspend(for {
        splitMaxResult <- rightAsNode.splitMax[B, S]
      } yield (DietNode(start, end, left, splitMaxResult._1), splitMaxResult._2))
    }
  }

  private[scalaz] def splitMin[B >: A, S[+_]](implicit E: Enum[B], S: Pointed[S]): Free[S, (Diet[B], (B, B))] = {
    left match {
      case DietEmpty => return_((right, (start, end)))
      case leftAsNode@ DietNode(_, _, _, _) => suspend(for {
        splitMinResult <- leftAsNode.splitMin[B, S]
      } yield (DietNode(start, end, splitMinResult._1, right), splitMinResult._2))
    }
  }

  private[scalaz] def joinLeft[B >: A, S[+_]](implicit E: Enum[B], S: Pointed[S]): Free[S, Diet[B]] = {
    left match {
      case DietEmpty => return_(this)
      case leftAsNode@ DietNode(_, _, _, _) => suspend(for {
        splitMaxResult <- leftAsNode.splitMax[B, S]
      } yield if ((splitMaxResult._2._2: B).succ == start) DietNode(splitMaxResult._2._1, end, splitMaxResult._1, right) else this)
    }
  }
  private[scalaz] def joinRight[B >: A, S[+_]](implicit E: Enum[B], S: Pointed[S]): Free[S, Diet[B]]  = {
    right match {
      case DietEmpty => return_(this)
      case rightAsNode@ DietNode(_, _, _, _) => suspend(for {
        splitMinResult <- rightAsNode.splitMin[B, S]
      } yield if ((end: B).succ == splitMinResult._2._1) DietNode(start, splitMinResult._2._2, left, splitMinResult._1) else this)
    }
  }
  
  def merge[B >: A](other: Diet[B])(implicit E: Enum[B]): Diet[B] = {
    other match {
      case DietEmpty => this
      case otherAsNode@ DietNode(_, _, _, _) => {
        val (otherLeft, (otherStart, otherEnd)) = otherAsNode.splitMax[B, Function0].go(_.copoint)
        DietNode(start, end, otherLeft, right)
      }
    }
  }

  def freeFoldLeft[B, S[+_]](b: B)(f: (B, (A, A)) => B)(implicit S: Pointed[S]): Free[S, B] = {
    for {
      fromLeft <- left.freeFoldLeft[B, S](b)(f)
      fromThis <- return_[S, B](f(fromLeft, (start, end)))
      fromRight <- right.freeFoldLeft[B, S](fromThis)(f)
    } yield fromRight
  }
}
