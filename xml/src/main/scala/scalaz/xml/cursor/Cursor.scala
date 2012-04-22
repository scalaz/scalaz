package scalaz
package xml
package cursor

sealed trait Cursor {
  import Cursor._

  val current: Content
  val lefts: List[Content]
  val rights: List[Content]
  val parents: Path

  import QName._

  def unary_- : Content =
    root.current

  // alias for current
  def unary_~ : Content =
    current

  /// setContent
  def setCurrent(c: Content): Cursor =
    cursor(c, lefts, rights, parents)

  /// setContent (alias)
  def !(c: Content): Cursor =
    setCurrent(c)

  /// modifyContent
  def withCurrent(c: Content => Content): Cursor =
    cursor(c(current), lefts, rights, parents)

  /// alias for withCurrent
  def -->>(c: Content => Content): Cursor =
    withCurrent(c)

  def setLefts(l: List[Content]): Cursor =
    cursor(current, l, rights, parents)

  def withLefts(l: List[Content] => List[Content]): Cursor =
    cursor(current, l(lefts), rights, parents)

  def setRights(l: List[Content]): Cursor =
    cursor(current, l, rights, parents)

  def withRights(l: List[Content] => List[Content]): Cursor =
    cursor(current, lefts, l(rights), parents)

  /// insertLeft
  def <<<:(c: Content): Cursor =
    withLefts(c :: _)

  /// insertRight
  def :>>>(c: Content): Cursor =
    withRights(c :: _)

  /// parent
  def parent: Option[Cursor] =
    unconsOption(parents) {
      case ((pls, v, prs), ps) =>
        cursor(
            current = Content.elem(v fromTag (combChildren(lefts, current, rights)))
          , lefts = pls
          , rights = prs
          , parents = ps
        )
    }

  // alias for parent
  def ^ : Option[Cursor] =
    parent

  def parentOr(c: => Cursor): Cursor =
    parent getOrElse c

  /// root
  def root: Cursor = {
    @annotation.tailrec
    def rooot(c: Cursor): Cursor =
      c.parent match {
        case None => c
        case Some(r) => rooot(r)
      }

    rooot(this)
  }

  /// isRoot
  def isRoot: Boolean =
    parents.isEmpty

  /// isFirst
  def isFirst: Boolean =
    lefts.isEmpty

  /// isLast
  def isLast: Boolean =
    rights.isEmpty

  /// isLeaf
  def isLeaf: Boolean =
    current.fold(
      _ => false
    , _ => true
    , _ => true
    , _ => false
    )

  /// getNodeIndex
  def nodeIndex: Int =
    lefts.length

  /// hasChildren
  def hasChildren: Boolean =
    !isLeaf

  /// isChild
  def isChild: Boolean =
    !isRoot

  /// left
  def left: Option[Cursor] =
    unconsOption(lefts)((t: Content, ts: List[Content]) => cursor(t, ts, current :: rights, parents))

  def leftOr(c: => Cursor): Cursor =
    left getOrElse c

  /// right
  def right: Option[Cursor] =
    unconsOption(rights)((t: Content, ts: List[Content]) => cursor(t, current :: lefts, ts, parents))

  def rightOr(c: => Cursor): Cursor =
    right getOrElse c

  /// firstChild
  def firstChild: Option[Cursor] =
    downParents flatMap {
      case (ts, ps) =>
        unconsOption(ts)((h: Content, t: List[Content]) => cursor(
          current = h,
          lefts = Nil,
          rights = t,
          parents = ps
        ))
    }

  def firstChildOr(c: => Cursor): Cursor =
    firstChild getOrElse c

  /// lastChild
  def lastChild: Option[Cursor] =
    downParents flatMap {
      case (ts, ps) =>
        unconsOption(ts.reverse)((h: Content, t: List[Content]) => cursor(
          current = h,
          lefts = t,
          rights = Nil,
          parents = ps
        ))
    }

  def lastChildOr(c: => Cursor): Cursor =
    lastChild getOrElse c


  /// findLeft
  def findLeft(p: Cursor => Boolean): Option[Cursor] =
    left flatMap (l => if (p(l)) Some(l) else l.findLeft(p))

  def findLeftOr(p: Cursor => Boolean, c: => Cursor): Cursor =
    findLeft(p) getOrElse c

  /// findRight
  def findRight(p: Cursor => Boolean): Option[Cursor] =
    right flatMap (r => if (p(r)) Some(r) else r.findRight(p))

  def findRightOr(p: Cursor => Boolean, c: => Cursor): Cursor =
    findRight(p) getOrElse c

  /// findChild
  def findChild(p: Cursor => Boolean): Option[Cursor] =
    firstChild flatMap (f => if (p(f)) Some(f) else f.findRight(p))

  def findChildOr(p: Cursor => Boolean, c: => Cursor): Cursor =
    findChild(p) getOrElse c

  def findChildElementQname(p: QName => Boolean): Option[Cursor] =
    findChild(c => c.current.elem exists (e => p(e.name)))

  def findChildElementQnameOr(p: QName => Boolean, c: => Cursor): Cursor =
    findChildElementQname(p) getOrElse c

  def findChildElementName(p: String => Boolean): Option[Cursor] =
    findChildElementQname(n => p(n.name.mkString))

  def findChildElementNameOr(p: String => Boolean, c: => Cursor): Cursor =
    findChildElementName(p) getOrElse c

  /// nextDF
  def nextDepthFirst: Option[Cursor] = {
    def up(x: Cursor): Option[Cursor] =
      x.right orElse (x.parent flatMap (up(_)))

    firstChild orElse up(this)
  }

  def nextDepthFirstOr(c: => Cursor): Cursor =
    nextDepthFirst getOrElse c

  /// findRec
  def findRec(p: Cursor => Boolean): Option[Cursor] =
    if(p(this))
      Some(this)
    else
      nextDepthFirst flatMap (_ findRec p)

  def findRecOr(p: Cursor => Boolean, c: => Cursor): Cursor =
    findRec(p) getOrElse c

  /// getChild
  def nthChild(n: => Int): Option[Cursor] =
    for {
      (ts, ps) <- downParents
      (ls, t, rs) <- splitChildren(ts, n)
    } yield cursor(
      current = t,
      lefts = ls,
      rights = rs,
      parents = ps
    )

  def getChildOr(n: => Int, c: => Cursor): Cursor =
    nthChild(n) getOrElse c

  /// toTree
  def toTree: Content =
    root.current
  
  /// toForest
  def toForest: List[Content] = {
    val r = root
    combChildren(r.lefts, r.current, r.rights)
  }

  // The new position is:
  // * right if exists, or
  // * left if exists, or
  // * parent
  def remove: Option[(Content, Cursor)] =
    rights match {
      case h::t => Some((current, setCurrent(h).setRights(t)))
      case Nil =>
        lefts match {
          case h::t => Some((current, setCurrent(h).setLefts(t)))
          case Nil => parent map (p => (current, removeRights.removeLefts))
        }
    }

  def removeOr(c: => (Content, Cursor)) =
    remove getOrElse c

  def iremove: Option[Cursor] =
    remove map (_._2)

  def iremoveOr(c: => Cursor): Cursor =
    iremove getOrElse c

  def removeLefts: Cursor =
    setLefts(Nil)

  def removeRights: Cursor =
    setRights(Nil)

  /// removeLeft
  def removeLeft: Option[(Content, Cursor)] =
    unconsOption(lefts) {
      case (l, ls) => (l, setLefts(ls))
    }

  def removeLeftOr(c: => (Content, Cursor)): (Content, Cursor) =
    removeLeft getOrElse c

  /// removeRight
  def removeRight: Option[(Content, Cursor)] =
    unconsOption(rights) {
      case (r, rs) => (r, setRights(rs))
    }

  def removeRightOr(c: => (Content, Cursor)): (Content, Cursor) =
    removeRight getOrElse c

  /// insertGoLeft
  def insertGoLeft(t: Content): Cursor =
    setCurrent(t) :>>> current

  /// insertGoRight
  def insertGoRight(t: Content): Cursor =
    current <<<: setCurrent(t)

  /// removeGoLeft
  def removeGoLeft(t: Content): Option[Cursor] =
    unconsOption(lefts) {
      case (h, t) => setCurrent(h).setLefts(t)
    }

  def removeGoLeftOr(t: Content, c: => Cursor) =
    removeGoLeft(t) getOrElse(c)

  // removeGoRight
  def removeGoRight(t: Content): Option[Cursor] =
    unconsOption(rights) {
      case (h, t) => setCurrent(h).setRights(t)
    }

  def removeGoRightOr(t: Content, c: => Cursor): Cursor =
    removeGoRight(t) getOrElse c

  /// removeGoUp
  def removeGoUp: Option[Cursor] =
    unconsOption(parents) {
      case ((pls, v, prs), ps) =>
        cursor(
          current = Content.elem(v.fromTag(lefts.reverse ::: rights))
        , lefts = pls
        , rights = prs
        , parents = ps
        )
    }

  def removeGoUpOr(c: => Cursor): Cursor =
    removeGoUp getOrElse c

  def elem: Option[Element] =
    current.elem

  def elemOr(e: => Element): Element =
    elem getOrElse e

  def isElem: Boolean =
    elem.isDefined

  def text: Option[CData] =
    current.text

  def textOr(d: => CData): CData =
    text getOrElse d

  def isText: Boolean =
    text.isDefined

  def cref: Option[Str] =
    current.cref

  def crefOr(s: => Str): Str =
    cref getOrElse s

  def isCref: Boolean =
    cref.isDefined

  def comment: Option[Str] =
    current.comment

  def commentOr(s: => Str): Str =
    comment getOrElse s

  def isComment: Boolean =
    comment.isDefined

  def usingElem(k: Element => Element): Cursor =
    withCurrent(_ usingElem k)

  def usingText(k: CData => CData): Cursor =
    withCurrent(_ usingText k)

  def usingCref(k: Str => Str): Cursor =
    withCurrent(_ usingCref k)

  def usingComment(k: Str => Str): Cursor =
    withCurrent(_ usingComment k)

  @annotation.tailrec
  final def walk(k: Cursor => Content): Cursor = {
    def up(c: Cursor): Option[Cursor] =
      c.parent flatMap(p => p.right orElse up(p))

    val x = setCurrent(k(this))
    x.firstChild orElse x.right orElse up(x) match {
      case None => x
      case Some(c) => c walk k
    }
  }

  /// splitChildren
  private def splitChildren[A](c: List[A], n: Int): Option[(List[A], A, List[A])] =
    if(n < 0)
      None
    else {
      @annotation.tailrec
      def loop(acc: List[A], z: List[A], w: Int): Option[(List[A], A, List[A])] =
        z match {
          case Nil =>
            None
          case x::xs =>
            if(w == 0)
              Some(acc, x, xs)
            else
              loop(x::acc, xs, n - 1)
        }

      loop(Nil, c, n)
    }

  /// combChildren
  private def combChildren[A](ls: List[A], t: A, rs: List[A]): List[A] =
    ls.foldLeft(t :: rs)((a, b) => b :: a)

  /// downParents
  private def downParents: Option[(List[Content], Path)] =
    current.elem map (e => (e.content, (lefts, e.tag, rights) :: parents))

  private def unconsOption[A, B](a: List[A])(f: (A, List[A]) => B): Option[B] =
    a match {
      case Nil => None
      case h :: t => Some(f(h, t))
    }

}

trait Cursors {
  type Path =
  List[(List[Content], Tag, List[Content])]

  def cursor(current: Content, lefts: List[Content] = Nil, rights: List[Content] = Nil, parents: Path = Nil): Cursor = {
    val c = current
    val l = lefts
    val r = rights
    val p = parents
    new Cursor {
      val current = c
      val lefts = l
      val rights = r
      val parents = p
    }
  }

  import std.AllInstances._

  implicit val CursorShow: Show[Cursor] = new Show[Cursor] {
    def show(c: Cursor) =
      ("Cursor{current=" + Show[Content].shows(c.current) + ",lefts=" + Show[List[Content]].shows(c.lefts) + ",rights=" + Show[List[Content]].shows(c.rights) + ",parents=" + Show[Path].shows(c.parents)).toList
  }

  implicit val CursorEqual: Equal[Cursor] =
    Equal.equalBy(c => (c.current, c.lefts, c.rights, c.parents))

}

object Cursor extends Cursors {

  import Lens._
  import CostateT._

  val currentCursorL: Cursor @> Content =
    lens(x => costate(b => cursor(b, x.lefts, x.rights, x.parents), x.current))

  val leftsCursorL: Cursor @> List[Content] =
    lens(x => costate(b => cursor(x.current, b, x.rights, x.parents), x.lefts))

  val rightsCursorL: Cursor @> List[Content] =
    lens(x => costate(b => cursor(x.current, x.lefts, b, x.parents), x.rights))

  val parentsCursorL: Cursor @> Path =
    lens(x => costate(b => cursor(x.current, x.lefts, x.rights, b), x.parents))

}
