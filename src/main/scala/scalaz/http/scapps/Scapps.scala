package scalaz.http.scapps

import Route._
import scala.xml.{Elem, Node, NodeSeq, Null}
import scalaz.Scalaz._
import scalaz.{Kleisli, OptionW, StringW, Zero}
import scalaz.http.request._

object Scapps {
  import _root_.scala.xml.NodeSeq

  def firstSome[A, B](fs: List[Kleisli[Option, A, B]])(a: A): Option[B] = (fs.elements.map(_(a)).find(_.isDefined)).join

  implicit def OptionKleisli[A, B](f: A => Option[B]): Kleisli[Option, A, B] = Kleisli.kleisli[Option](f)

  implicit def ListKleisliKleisli[A, B](fs: List[Kleisli[Option, A, B]]): Kleisli[Option, A, B] = Kleisli.kleisli[Option](firstSome(fs) _)

  implicit def PathKliesli(s: String) = Kleisli.kleisli[Option](exactPath(s))

  implicit def MethodKliesli(m: Method) = Kleisli.kleisli[Option](methodM(m))

  implicit def PathMethodKliesli(t: Tuple2[Method, String]) = MethodKliesli(t._1) >=> PathKliesli(t._2)

  implicit def NodeSeqZero: Zero[NodeSeq] = new Zero[NodeSeq] {val zero = xml.NodeSeq.Empty}

  implicit def NodeZero: Zero[Node] = new Zero[Node] {
    val zero = new Node {
      override def text = null
      override def label = null
      override def child = Nil
    }
  }

  implicit def ElemZero: Zero[Elem] = new Zero[Elem] {
    val zero = new Elem(null, null, Null, xml.TopScope, Nil: _*)
  }
}

