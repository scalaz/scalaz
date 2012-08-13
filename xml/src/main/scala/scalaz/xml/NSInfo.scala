package scalaz
package xml

import QName._

sealed trait NSInfo {
  val prefixes: List[(Str, Str)]
  val uri: Option[Str]

  import NSInfo._
  import Attr._
  import Element._
  import Content._
  import CData._
  import CDataKind._

  /// addNS
  def +:(a: Attr): NSInfo =
    (a.key.prefix, a.key.name) match {
      case (None, List('x', 'm', 'l', 'n', 's')) => nsInfo(prefixes, if(a.value.isEmpty) None else Some(a.value))
      case (Some(List('x', 'm', 'l', 'n', 's')), k) => nsInfo((k, a.value) :: prefixes, uri)
      case _ => this
    }

  def +:+(a: Attr): NSInfo =
    +:(a)

  /// annotName
  def annotName(n: QName): QName =
    qname(
      name = n.name
    , uri = n.prefix match {
        case None => uri
        case Some(w) =>
          prefixes.collectFirst{ case (`w`,x) => x }
      }
    , prefix = n.prefix
    )

  /// annotAttr
  def annotAttr(a: Attr): Attr =
    (a.key.prefix, a.key.name) match {
      case (None, _) => a
      case _ => attr(annotName(a.key), a.value)
    }

  /// nodes
  def nodes(ps: List[QName], ts: List[Token]): (List[Content], List[QName], List[Token]) =
    nodesT(ps, ts).run

  import Free._

  // why can't I import std.function._? compiler -> StackOverflowError
  private[xml] implicit val Function0Functor: Functor[Function0] = new Functor[Function0] {
    def map[A, B](fa: Function0[A])(f: A => B) =
      () => f(fa())
  }

  private[xml] def nodesT(ps: List[QName], ts: List[Token]): Trampoline[(List[Content], List[QName], List[Token])] =
    ts match {
      case t::u =>
        t.fold(
          start = p => t => as => e => {
            val newInfo = as.foldLeft(this)(_ +:+ _)
            val newName = newInfo annotName t
            val r =
              if(e)
                nodesT(ps, u) map ((Nil, _))
              else {
                newInfo.nodesT(newName::ps, u) flatMap {
                  case (es1, qs1, ts1) => {
                    val z =
                      qs1 match {
                        case Nil => nodesT(ps, ts1)
                        case _::q => Return[Function0, (List[Content], List[QName], List[Token])](Nil, q, ts1)
                      }
                    z map ((es1, _))
                  }
                }
              }

            r map {
              case (children, (siblings, open, toks)) => {
                val node = elem(element(
                                         name = newName
                                       , attribs = as map (newInfo annotAttr _)
                                       , content = children
                                       , line = Some(p)
                                       )
                               )

                (node :: siblings, open, toks)
              }
            }
          }
        , end = p => t => {
            val t1 = annotName(t)
            ps span (q => !Equal[QName].equal(q, t1)) match {
              case (as, _::_) => Return[Function0, (List[Content], List[QName], List[Token])](Nil, as, u)
              case (_, Nil) => {
                nodesT(ps, u) map {
                  case (es, qs, ts1) =>
                    (text(cdata(
                      verbatim = cdataText
                    , data = '<' :: '/' :: pp.XPrint.showQname(t) ::: List('>')
                    , line = Some(p)
                    )) :: es, qs, ts1)
                }
              }
            }
          }
        , cref = ref =>
            nodesT(ps, u) map {
              case (es, qs, ts1) =>
                (cref(ref) :: es, qs, ts1)
            }
        , text = txt =>
            nodesT(ps, u) map {
              case (es, qs, ts1) => {
                val (more, es1) = es match {
                  case Nil =>
                    (Nil, es)
                  case w::ws =>
                    w.text filter (cd => Equal[CDataKind].equal(cd.verbatim, txt.verbatim)) match {
                      case Some(cd) => (cd.data, ws)
                      case None => (Nil, es)
                    }
                  }
                (text(txt.setData(txt.data ::: more)) :: es1, qs, ts1)
              }
            }
        , comment = s => {
            nodesT(ps, u) map {
              case (es, qs, ts1) =>
                (comment(s) :: es, qs, ts1)
            }
          }
        )

      case Nil =>
        Return[Function0, (List[Content], List[QName], List[Token])](Nil, ps, Nil)
    }

}

trait NSInfos {
  def nsInfo(prefixes: List[(Str, Str)], uri: Option[Str] = None): NSInfo = {
    val p = prefixes
    val u = uri
    new NSInfo {
      val prefixes = p
      val uri = u
    }
  }

  import std.AllInstances._

  implicit val NSInfoShow: Show[NSInfo] = new Show[NSInfo] {
    override def shows(n: NSInfo) =
      ("NSInfo{prefixes=" + Show[List[(Str, Str)]].shows(n.prefixes) + (n.uri match {
        case None => ""
        case Some(u) => ",uri=" + u
      }) + "}")
  }

  implicit val NSInfoEqual: Equal[NSInfo] =
    Equal.equalBy[NSInfo, (List[(Str, Str)], Option[Str])](n => (n.prefixes, n.uri))

}

object NSInfo extends NSInfos {

  import Lens._
  import StoreT._

  val prefixesNSInfoL: NSInfo @> List[(Str, Str)] =
    lens(x => store(x.prefixes)(b => nsInfo(b, x.uri)))

  val uriNSInfoL: NSInfo @> Option[Str] =
    lens(x => store(x.uri)(b => nsInfo(x.prefixes, b)))

}
