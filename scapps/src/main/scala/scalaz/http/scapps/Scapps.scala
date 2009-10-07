package scalaz.http.scapps

import Routing._
import scala.xml.{Elem, Node, NodeSeq, Null}
import scalaz.Scalaz._
import scalaz.{Kleisli, OptionW, StringW, Zero}
import scalaz.http.request._
import scalaz.MA

trait PartialApplySomething[T[_[_], _, _], M[_], A] {
  type Apply[B] = T[M, A, B]
}


trait Scapps {
  import _root_.scala.xml.NodeSeq

  import Kleisli.kleisli
  import Zero.zero
  import Semigroup.semigroup

  // Check in 2.8, see if we can generalise Option[B]
  implicit def OptionKleisli[A, B](f: A => Option[B]): Kleisli[Option, A, B] = kleisli[Option](f)
  
  // Check in 2.8, see if we can generalise Option[B]
  implicit def OptionKleisliZero[A, B](implicit z : Zero[Option[B]]) : Zero[Kleisli[Option, A, B]] = {
    zero(kleisli[Option]((_ : A) => z.zero))
  }

  // Check in 2.8, see if we can generalise Option[B]  
  implicit def OptionKleisliSemigroup[A, B](implicit sg : Semigroup[Option[B]]) : Semigroup[Kleisli[Option, A, B]] = {
    semigroup[Kleisli[Option, A, B]]((k1, k2) => {
      kleisli[Option]((a : A) => {
        sg.append(k1(a), { val k = k2; k(a) })
      })
    })
  }
  
  implicit def OptionKleisliMA[R, A](k: Kleisli[Option, R, A]) = MA.ma[PartialApplySomething[Kleisli, Option, R]#Apply](k)
  
  implicit def OptionKleisliFunctor[X] = new Functor[PartialApplySomething[Kleisli, Option, X]#Apply] {
    def fmap[A, B](r: Kleisli[Option, X, A], f: A => B) = {
      kleisli[Option]((x : X) => r(x) map f)
    }
  }
  
  implicit def PathKliesli(s: String) = kleisli[Option](exactPath(s))

  implicit def MethodKliesli(m: Method) = kleisli[Option](methodM(m))

  implicit def PathMethodKliesli(t: Tuple2[Method, String]) = MethodKliesli(t._1) >=> PathKliesli(t._2)

  // TODO These zeros should be in Zero.scala
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

object Scapps extends Scapps