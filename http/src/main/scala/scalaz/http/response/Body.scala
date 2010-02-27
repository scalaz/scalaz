package scalaz
package http
package response

/**
 * Type constructors that can be used in request and response bodies.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait Body[OUT[_], A] {
  /**
   * Take the given value to a request/response body value.
   */
  def apply(a: A): OUT[Byte]
}

import Scalaz._
import scala.xml.{Elem, NodeSeq}

trait Bodys {
  import Body._
  /**
   * The body implementation for stream identity.
   */
  implicit val StreamIdentityBody = identityBody[Stream]

  /**
   * The body implementation for a stream of characters to a stream.
   */
  implicit def CharStreamBody(implicit c: CharSet) = body[Stream]((_: Stream[Char]).mkString.getBytes(c.value).toStream)

  /**
   * The body implementation for a list to a stream.
   */
  implicit val ByteListStreamBody: Body[Stream, List[Byte]] = body[Stream](_.toStream)

  /**
   * The body implementation for a list of characters to a stream.
   */
  implicit def CharListStreamBody(implicit c: CharSet): Body[Stream, List[Char]] = body[Stream](_.mkString.getBytes(c.value).toStream)

  /**
   * The body implementation for a list of characters to a stream.
   */
  implicit def StringStreamBody(implicit c: CharSet): Body[Stream, String] = body[Stream](_.getBytes(c.value).toStream)

  /**
   * The body implementation for an XML element to a stream.
   */
  implicit def ElemStreamBody(implicit c: CharSet): Body[Stream, Elem] = body[Stream](_.toString.getBytes(c.value).toStream)

  /**
   * The body implementation for an XML element to a stream.
   */
  implicit def NodeSeqStreamBody(implicit c: CharSet): Body[Stream, NodeSeq] = body[Stream](_.toString.getBytes(c.value).toStream)


  /**
   * The body implementation for an XHTML doctype to a stream.
   */
  implicit def XhtmlDoctypeStreamBody(implicit c: CharSet): Body[Stream, xhtml.Doctype] = body[Stream](_.asString.getBytes(c.value).toStream)

  
//  /**
//   * The cofunctor implementation for a body.
//   */
//  def bodyCofunctor[OUT[_]]: Cofunctor[PartialType2[Body, OUT]#Apply] = new Cofunctor[PartialType2[Body, OUT]#Apply] {
//    def comap[A, B](body: PartialType2[Body, OUT]#Apply[A], f: B => A): Body[OUT, B] = new Body[OUT, B] {
//      def apply(b: B) = body(f(b))
//    }
//  }

  /**
   * The cofunctor wrapper implementation for a body.
   */
  //def bodyCofunctorW[OUT[_], A](c: PartialType2[Body, OUT]#Apply[A]): scalaz.CofunctorW[PartialType2[Body, OUT]#Apply, A] = cofunctor[PartialType2[Body, OUT]#Apply](c)(bodyCofunctor[OUT])

  /**
   * The cofunctor implementation for a stream body.
   */
//  implicit val StreamBodyCofunctor = bodyCofunctor[Stream]

  /**
   * The cofunctor wrapper implementation for a stream body.
   */
  //implicit def StreamBodyCofunctorW[A](c: PartialType2[Body, Stream]#Apply[A]) = bodyCofunctorW[Stream, A](c)
}
/**
 * Type constructors that can be used in request and response bodies.
 */
object Body extends Bodys {
  /**
   * Construct a body implementation from the given function.
   */
  def body[OUT[_]] = new {
    def apply[A](f: A => OUT[Byte]) = new Body[OUT, A] {
      def apply(a: A) = f(a)
    }
  }

  /**
   * The body implementation for identity.
   */
  def identityBody[OUT[_]] = body[OUT](identity[OUT[Byte]])

  /**
   * The body implementation for stream.
   */
  def streamBody[A](f: A => Stream[Byte]) = body[Stream](f)
}
