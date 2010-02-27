package scalaz
package http
package request

import Util.Nel._
import Scalaz._

/**
 * HTTP request URI.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2">RFC 2616 Section 5.1.2 Request-URI</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 */
sealed trait Uri {
  /**
   * The path of this request URI.
   */
  val path: NonEmptyList[Char]

  /**
   * The query string of this request URI.
   */
  val queryString: Option[List[Char]]

  import Uri.uri

  /**
   * Returns a request URI with the given path and this query string.
   */
  def apply(p: NonEmptyList[Char]) = uri(p, queryString)

  /**
   * Returns a request URI with the given potential query string and this path.
   */
  def apply(q: Option[List[Char]]) = uri(path, q)

  /**
   * Returns a request URI after applying the given transformation to the path.
   */
  def +++(f: NonEmptyList[Char] => NonEmptyList[Char]) = uri(f(this.path), queryString)

  /**
   * Returns a request URI after applying the given transformation to the query string.
   */
  def ++++(f: Option[List[Char]] => Option[List[Char]]) = uri(path, f(this.queryString))

  /**
   * Returns the path extension - characters after the last dot (.) in the path.
   */
  lazy val pathExtension = path.dropWhile(_ != '.').reverse.takeWhile(_ != '.').reverse.mkString
  
  lazy val parts : List[String] = {
    path.list.reverse.foldLeft(List[List[Char]](Nil))((rs, ch) => {
      if (ch == '/') { Nil :: rs } else { (ch :: rs.head)  :: rs.tail }
    }).filter(!_.isEmpty).map(_.mkString)
  }

  import Util.{asHashMap, mapHeads}

  /**
   * Returns the query string split into values by <code>'='</code>.
   */
  lazy val parameters = queryString ∘ (Util.parameters(_))

  /**
   * Returns the query string split into values by <code>'='</code> backed with a hash map.
   */
  lazy val parametersMap = parameters ∘ (asHashMap[List, NonEmptyList](_))

  /**
   * Returns the query string split into values by <code>'='</code> (removing duplicate values for a given key) backed
   * with a hash map.
   */
  lazy val parametersMapHeads = parametersMap ∘ (mapHeads(_))
}

trait Uris {
  /**
   * Takes the given string and splits it into a URI and query string by <code>'?'</code> character.
   */
  implicit def ListUri(cs: List[Char]): Option[Uri] = cs match {
    case Nil => None
    case x :: _ if x == '?' => None
    case h :: t => {
      val z = t.span(_ != '?')
      Some(Uri.uri(nel(h, z._1), z._2 match {
        case Nil => None
        case _ :: Nil => None
        case _ :: k => Some(k)
      }))
    }
  }
}

/*
 * HTTP request URI.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2">RFC 2616 Section 5.1.2 Request-URI</a>.
 */
object Uri extends Uris {
  /**
   * An extractor that always matches with the URI path and query string.
   */
  def unapply(uri: Uri): Option[(NonEmptyList[Char], Option[List[Char]])] =
    Some(uri.path, uri.queryString)

  /**
   * Constructs a URI with the given path and query string.
   */
  def uri(p: NonEmptyList[Char], s: Option[List[Char]]) = new Uri {
    val path = p
    val queryString = s
  }
}
