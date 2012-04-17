package scalaz
package xml
package cursor

import Predicate._
import Op._

sealed trait Op {
  def fold[X](
    choiceSucceed: History => X
  , choiceSwitch: History => History => X
  , failedCompose: => X
  , left: => X
  , right: => X
  , firstChild: => X
  , lastChild: => X
  , remove: => X
  , removeLeft: => X
  , removeRight: => X
  , parent: => X
  , root: => X
  , findLeft: CPredicate => X
  , findRight: CPredicate => X
  , findChild: CPredicate => X
  , nextDepthFirst: => X
  , findRec: CPredicate => X
  , nthChild: (=> Int) => X
  , succeeding: (Cursor => Cursor) => OpDescription => X
  , generic: (Cursor => Option[Cursor]) => OpDescription => X
  ): X =
    this match {
      case ChoiceSucceed(h) => choiceSucceed(h)
      case ChoiceSwitch(o, n) => choiceSwitch(o)(n)
      case FailedCompose => failedCompose
      case LeftOp => left
      case RightOp => right
      case FirstChild => firstChild
      case LastChild => lastChild
      case Remove => remove
      case RemoveLeft => removeLeft
      case RemoveRight => removeRight
      case Parent => parent
      case Root => root
      case FindLeft(p) => findLeft(p)
      case FindRight(p) => findRight(p)
      case FindChild(p) => findChild(p)
      case NextDepthFirst => nextDepthFirst
      case FindRec(p) => findRec(p)
      case NthChild(n) => nthChild(n())
      case Succeeding(o, d) => succeeding(o)(d)
      case Generic(o, d) => generic(o)(d)
    }

  def choiceSucceed: Option[History] =
    this match {
      case ChoiceSucceed(h) => Some(h)
      case _ => None
    }

  def isChoiceSucceed: Boolean =
    choiceSucceed.isDefined

  def choiceSwitch: Option[(History, History)] =
    this match {
      case ChoiceSwitch(o, n) => Some(o, n)
      case _ => None
    }

  def isChoiceSwitch: Boolean =
    choiceSwitch.isDefined

  def isFailedCompose: Boolean =
    this match {
      case FailedCompose => true
      case _ => false
    }

  def isLeft: Boolean =
    this match {
      case LeftOp => true
      case _ => false
    }

  def isRight: Boolean =
    this match {
      case RightOp => true
      case _ => false
    }

  def isFirstChild: Boolean =
    this match {
      case FirstChild => true
      case _ => false
    }

  def isLastChild: Boolean =
    this match {
      case LastChild => true
      case _ => false
    }

  def isRemoveLeft: Boolean =
    this match {
      case RemoveLeft => true
      case _ => false
    }

  def isRemoveRight: Boolean =
    this match {
      case RemoveRight => true
      case _ => false
    }

  def isParent: Boolean =
    this match {
      case Parent => true
      case _ => false
    }

  def isRoot: Boolean =
    this match {
      case Root => true
      case _ => false
    }

  def findLeft: Option[CPredicate] =
    this match {
      case FindLeft(p) => Some(p)
      case _ => None
    }

  def isFindLeft: Boolean =
    findLeft.isDefined

  def findRight: Option[CPredicate] =
    this match {
      case FindRight(p) => Some(p)
      case _ => None
    }

  def isFindRight: Boolean =
    findRight.isDefined

  def findChild: Option[CPredicate] =
    this match {
      case FindChild(p) => Some(p)
      case _ => None
    }

  def isFindChild: Boolean =
    findChild.isDefined

  def isNextDepthFirst: Boolean =
    this match {
      case NextDepthFirst => true
      case _ => false
    }

  def findRec: Option[CPredicate] =
    this match {
      case FindRec(p) => Some(p)
      case _ => None
    }

  def isFindRec: Boolean =
    findRec.isDefined

  def nthChild: Option[Int] =
    this match {
      case NthChild(n) => Some(n())
      case _ => None
    }

  def isNthChild: Boolean =
    nthChild.isDefined

  def succeeding: Option[(Cursor => Cursor, OpDescription)] =
    this match {
      case Succeeding(o, d) => Some(o, d)
      case _ => None
    }

  def isSucceeding: Boolean =
    succeeding.isDefined

  def generic: Option[(Cursor => Option[Cursor], OpDescription)] =
    this match {
      case Generic(o, d) => Some(o, d)
      case _ => None
    }

  def isGeneric: Boolean =
    generic.isDefined

  def +++(c: Option[Cursor]): HCursor =
    HCursor.hcursor(History(this), c)
}
private case class ChoiceSucceed(c: History) extends Op
private case class ChoiceSwitch(old: History, n: History) extends Op
private case object FailedCompose extends Op
private case object LeftOp extends Op
private case object RightOp extends Op
private case object FirstChild extends Op
private case object LastChild extends Op
private case object Remove extends Op
private case object RemoveLeft extends Op
private case object RemoveRight extends Op
private case object Parent extends Op
private case object Root extends Op
private case class FindLeft(p: CPredicate) extends Op
private case class FindRight(p: CPredicate) extends Op
private case class FindChild(p: CPredicate) extends Op
private case object NextDepthFirst extends Op
private case class FindRec(p: CPredicate) extends Op
private case class NthChild(n: () => Int) extends Op
private case class Succeeding(e: Cursor => Cursor, description: OpDescription) extends Op
private case class Generic(e: Cursor => Option[Cursor], description: OpDescription) extends Op

trait Ops {
  type OpDescription =
  String

  import std.AllInstances._

  implicit val OpShow: Show[Op] = new Show[Op] {
    def show(x: Op) =
      (x match {
        case ChoiceSucceed(h) => "choice-succeed" + implicitly[Show[History]].shows(h)
        case ChoiceSwitch(o, n) => implicitly[Show[History]].shows(o) + " >choice-switch< " + implicitly[Show[History]].shows(n)
        case FailedCompose => "failed-compose"
        case LeftOp => "left"
        case RightOp => "right"
        case FirstChild => "first-child"
        case LastChild => "last-child"
        case Remove => "remove"
        case RemoveLeft => "remove-left"
        case RemoveRight => "remove-right"
        case Parent => "parent"
        case Root => "root"
        case FindLeft(p) => "find-left" + (p.name match {
          case None => ""
          case Some(n) => "{predicate=" + n.mkString + "}"
        })
        case FindRight(p) => "find-right" + (p.name match {
          case None => ""
          case Some(n) => "{predicate=" + n.mkString + "}"
        })
        case FindChild(p) => "find-child" + (p.name match {
          case None => ""
          case Some(n) => "{predicate=" + n.mkString + "}"
        })
        case NextDepthFirst => "next-depth-first"
        case FindRec(p) => "find-rec" + (p.name match {
          case None => ""
          case Some(n) => "{predicate=" + n.mkString + "}"
        })
        case NthChild(n) => "nth-child{" + n() + "}"
        case Succeeding(_, d) => "succeeding{description=" + d + "}"
        case Generic(_, d) => "generic{description=" + d + "}"
      }).toList
  }

  implicit val OpEqual: Equal[Op] = new Equal[Op] {
    def equal(x1: Op, x2: Op) =
      (x1, x2) match {
        case (ChoiceSucceed(c), ChoiceSucceed(d)) => implicitly[Equal[History]].equal(c, d)
        case (ChoiceSwitch(n1, o1), ChoiceSwitch(n2, o2)) => implicitly[Equal[History]].equal(n1, n2) && implicitly[Equal[History]].equal(o1, o2)
        case (FailedCompose, FailedCompose) => true
        case (LeftOp, LeftOp) => true
        case (RightOp, RightOp) => true
        case (FirstChild, FirstChild) => true
        case (LastChild, LastChild) => true
        case (Remove, Remove) => true
        case (RemoveLeft, RemoveLeft) => true
        case (RemoveRight, RemoveRight) => true
        case (Parent, Parent) => true
        case (Root, Root) => true
        case (FindLeft(p), FindLeft(q)) => implicitly[Equal[CPredicate]].equal(p, q)
        case (FindRight(p), FindRight(q)) => implicitly[Equal[CPredicate]].equal(p, q)
        case (FindChild(p), FindChild(q)) => implicitly[Equal[CPredicate]].equal(p, q)
        case (NextDepthFirst, NextDepthFirst) => true
        case (FindRec(p), FindRec(q)) => implicitly[Equal[CPredicate]].equal(p, q)
        case (NthChild(n), NthChild(o)) => implicitly[Equal[Int]].equal(n(), o())
        case (Succeeding(_, d), Succeeding(_, e)) => implicitly[Equal[OpDescription]].equal(d, e)
        case (Generic(_, d), Generic(_, e)) => implicitly[Equal[OpDescription]].equal(d, e)
        case (_, _) => false
      }
  }

  def choiceSucceedOp(h: History): Op =
    ChoiceSucceed(h)

  def choiceSwitchOp(n: History, o: History): Op =
    ChoiceSwitch(n, o)

  def failedComposeOp: Op =
    FailedCompose

  def leftOp: Op =
    LeftOp

  def rightOp: Op =
    RightOp

  def firstChildOp: Op =
    FirstChild

  def lastChildOp: Op =
    LastChild

  def remove: Op =
    Remove

  def removeLeftOp: Op =
    RemoveLeft

  def removeRightOp: Op =
    RemoveRight

  def parentOp: Op =
    Parent

  def rootOp: Op =
    Root

  def findLeftOp(p: CPredicate): Op =
    FindLeft(p)

  def findRightOp(p: CPredicate): Op =
    FindRight(p)

  def findChildOp(p: CPredicate): Op =
    FindChild(p)

  def nextDepthFirstOp: Op =
    NextDepthFirst

  def findRecOp(p: CPredicate): Op =
    FindRec(p)

  def nthChildOp(n: => Int): Op =
    NthChild(() => n)

  def succeedingOp(e: Cursor => Cursor, description: OpDescription): Op =
    Succeeding(e, description)

  def genericOp(e: Cursor => Option[Cursor], description: OpDescription): Op =
    Generic(e, description)

}

object Op extends Ops {
  import PLens._
  import CostateT._

  val choiceSucceedOpPL: Op @?> History =
    plens(_.choiceSucceed map (e => costate(choiceSucceedOp(_), e)))

  val choiceSwitchOpPL: Op @?> (History, History) =
    plens(_.choiceSwitch map (e => costate(h => choiceSwitchOp(h._1, h._2), e)))

  val findLeftOpPL: Op @?> CPredicate =
    plens(_.findLeft map (e => costate(findLeftOp(_), e)))

  val findRightOpPL: Op @?> CPredicate =
    plens(_.findRight map (e => costate(findRightOp(_), e)))

  val findChildOpPL: Op @?> CPredicate =
    plens(_.findChild map (e => costate(findChildOp(_), e)))

  val findRecOpPL: Op @?> CPredicate =
    plens(_.findRec map (e => costate(findRecOp(_), e)))

  val nthChildOpPL: Op @?> Int =
    plens(_.nthChild map (e => costate(nthChildOp(_), e)))

  val succeedingOpPL: Op @?> (Cursor => Cursor, OpDescription) =
    plens(_.succeeding map (e => costate(x => succeedingOp(x._1, x._2), e)))

  val genericOpPL: Op @?> (Cursor => Option[Cursor], OpDescription) =
    plens(_.generic map (e => costate(x => genericOp(x._1, x._2), e)))

}
