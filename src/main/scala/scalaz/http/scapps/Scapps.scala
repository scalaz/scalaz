package scalaz.http.scapps

import Route._
import scalaz.Scalaz._
import scalaz.{Kleisli, OptionW, StringW, Zero}
import scalaz.http.request._
import xml.{NodeSeq, Elem, Node}

object Scapps {

  def firstSome[A, B](fs: List[Kleisli[Option, A, B]])(a: A): Option[B] = (fs.elements.map(_(a)).find(_.isDefined)).join

  implicit def OptionKleisli[A, B](f: A => Option[B]): Kleisli[Option, A, B] = Kleisli.kleisli[Option](f)

  implicit def ListKleisliKleisli[A, B](fs: List[Kleisli[Option, A, B]]): Kleisli[Option, A, B] = Kleisli.kleisli[Option](firstSome(fs) _)

  implicit def PathKliesli(s: String) = Kleisli.kleisli[Option](exactPath(s))

  implicit def MethodKliesli(m: Method) = Kleisli.kleisli[Option](methodM(m))

  implicit def PathMethodKliesli(t: Tuple2[Method, String]) = MethodKliesli(t._1) >=> PathKliesli(t._2)

  implicit def NodeSeqZero: Zero[NodeSeq] = new Zero[NodeSeq] {val zero = xml.NodeSeq.Empty}

  implicit def ElemZero: Zero[Elem] = new Zero[Elem] {val zero = <span> </span>}
}

