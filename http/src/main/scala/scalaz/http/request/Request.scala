package scalaz.http.request

import scalaz.Scalaz._
import scalaz.http.Util.{asHashMap, mapHeads, parameters}
import scalaz.http.Util.Nel._
import scalaz.http._
import scalaz._

/**
 * HTTP request.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5">RFC 2616 Section 5 Request</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision<br>
 *          $LastChangedDate: 2009-06-24 20:48:22 +1000 (Wed, 24 Jun 2009) $<br>
 *          $LastChangedBy: tonymorris $
 */
sealed trait Request[IN[_]] {
  /**
   * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1">§</a>
   */
  val line: Line

  /**
   * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.3">§</a>
   */
  val headers: List[(RequestHeader, NonEmptyList[Char])]

  /**
   * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.3">§</a>
   */
  val body: IN[Byte]

  import Request.request

  /**
   * Adds the given request header and value to a request.
   */
  def apply(h: RequestHeader, v: NonEmptyList[Char]) = request[IN](line, (h, v) :: headers, body)

  /**
   * Sets the status line of a request.
   */
  def apply(l: Line): Request[IN] = request[IN](l, headers, body)

  /**
   * Returns the first value that corresponds to the given request header.
   */
  def apply(h: RequestHeader) = headersMapHeads.get(h)

  /**
   * Sets the request method of the status line of a request.
   */
  def apply(m: Method): Request[IN] = apply(line(m))

  /**
   * Sets the request URI of the status line of a request.
   */
  def apply(u: Uri): Request[IN] = apply(line(u))

  /**
   * Sets the request version of the status line of a request.
   */
  def apply(v: Version): Request[IN] = apply(line(v))

  /**
   * Sets the body of a request.
   */
  def >>(b: IN[Byte]) = request[IN](line, headers, b)

  /**
   * Appends the given value to the body of a request.
   */
  def |+|(b: IN[Byte])(implicit s: Semigroup[IN[Byte]]) = request[IN](line, headers, s.append(body, b))

  /**
   * Deletes all headers of the given value from a request.
   */
  def -(h: RequestHeader) = request[IN](line, headers filter { case (k, _) => h != k }, body)

  /**
   * The user-agent request header value.
   */
  def userAgent = this(UserAgent)
  
  /**
   * A map of request header values offering optimal seek time.
   */
  lazy val headersMap = asHashMap[List, NonEmptyList](headers)
  
  /**
   * A map of the first request header values offering optimal seek time.
   */
  lazy val headersMapHeads = mapHeads(headersMap)

  private val m = immutableHashMapMemo[FoldLeft[IN], List[(List[Char], List[Char])]]

  import scalaz.MA
  /**
   * Provides look up for POST request parameters in the request body. Only the first invocation uses the given
   * fold-left and subsequent invocations look-up using a memoisation table (scoped to each request).
   */
  def post(implicit f: FoldLeft[IN]) = new {
    val parameters = m(f => Util.parameters(ma(body).listl(f) map (_.toChar)))(f)
    lazy val parametersMap = asHashMap[List, NonEmptyList](parameters)
    lazy val parametersMapHeads = mapHeads(parametersMap)
    def |(p: String) = parametersMapHeads.get(p.toList)
    def ||(p: String): List[List[Char]] = parametersMap.get(p.toList)
  }

  /**
   * The request URI parameters.
   */
  def parametersMap = line.uri.parametersMap

  /**
   * The request URI parameters with only the first occurring value.
   */
  def parametersMapHeads = line.uri.parametersMapHeads

  /**
   *  Returns the first occurrence of the given request parameter in the request URI.
   */
  def !(p: String) = line.uri.parametersMapHeads ∗ (_.get(p.toList))

  /**
   *  Returns the first occurrence of a given request parameter in the request URI.
   */
  def ^!^ : Kleisli[Option, String, List[Char]] = kleisli(this ! (_: String))

  /**
   * Returns the first occurrence of the given request parameter in the request URI or the given error value.
   */
  def ![E](p: String, e: => E): Validation[E, List[Char]] = this ! p toSuccess e

  /**
   * Returns all occurrences of the given request parameter in the request URI.
   */
  def !!(p: String) : List[List[Char]] = OptionNonEmptyListList(line.uri.parametersMap ∗ (_.get(p.toList)))

  /**
   * Returns all occurrences of a given request parameter in the request URI.
   */
  def ^!!^ : Kleisli[Option, String, NonEmptyList[List[Char]]] = kleisli((p : String) => {(this !! p).nel })

  /**
   * Returns all occurrences of the given request parameter in the request URI or the given error value.
   */
  def !![E](p: String, e: => E): Validation[E, NonEmptyList[List[Char]]] = (this !! p).nel toSuccess e

  /**
   * Returns <code>true</code> if the given request parameter occurs in the request URI.
   */
  def !?(p: String) = this ! p isDefined

  /**
   * Returns <code>false</code> if the given request parameter occurs in the request URI.
   */
  def ~!?(p: String) = this ! p isEmpty

  /**
   * Returns the first occurrence of the given request parameter in the request body.
   */
  def |(p: String)(implicit f: FoldLeft[IN]) = post | p

  /**
   *  Returns the first occurrence of a given request parameter in the request body.
   */
  def ^|^(implicit f: FoldLeft[IN]) : Kleisli[Option, String, List[Char]] = kleisli(this | (_: String))

  /**
   * Returns <code>true</code> if the given request parameter occurs in the request body.
   */
  def |?(p: String)(implicit f: FoldLeft[IN]) = this | p isDefined

  /**
   * Returns <code>false</code> if the given request parameter occurs in the request body.
   */
  def ~|?(p: String)(implicit f: FoldLeft[IN]) = this | p isEmpty

  /**
   * Returns all occurrences of the given request parameter in the request body.
   */
  def ||(p: String)(implicit f: FoldLeft[IN]) : List[List[Char]] = post || p

  /**
   *  Returns all occurrences of a given request parameter in the request body.
   */
  def ^||^(implicit f: FoldLeft[IN]) : Kleisli[Option, String, NonEmptyList[List[Char]]] =
    kleisli((p : String) => {(this || p).nel })

  /**
   * Returns the first occurrence of the given request parameter in the request URI if it exists or in the request body
   * otherwise.
   */
  def !|(p: String)(implicit f: FoldLeft[IN]) = this.!(p) ⟴ |(p)

  /**
   *  Returns the first occurrence of a given request parameter in the request URI if it exists or in the request body
   * otherwise.
   */
  def ^!|^(implicit f: FoldLeft[IN]) : Kleisli[Option, String, List[Char]] =
    kleisli((this !| (_: String)))

  /**
   * Returns the first occurrence of the given request parameter in the request body if it exists or in the request URI
   * otherwise.
   */
  def |!(p: String)(implicit f: FoldLeft[IN]) = |(p) ⟴ this.!(p)

  /**
   * Returns the first occurrence of a given request parameter in the request body if it exists or in the request URI
   * otherwise.
   */
  def ^|!^(implicit f: FoldLeft[IN]) : Kleisli[Option, String, List[Char]] = kleisli(this |! (_: String))

  /**
   * Returns all occurrences of the given request parameter in the request URI if it exists or in the request body
   * otherwise.
   */
  def !!||(p: String)(implicit f: FoldLeft[IN]) = this.!!(p) ⟴ ||(p)

  /**
   * Returns all occurrences of a given request parameter in the request URI if it exists or in the request body
   * otherwise.
   */
  def ^!!||^(implicit f: FoldLeft[IN]) : Kleisli[Option, String, NonEmptyList[List[Char]]] =
    kleisli((p : String) => {(this !!|| p).nel })

  /**
   * Returns all occurrences of the given request parameter in the request body if it exists or in the request URI
   * otherwise.
   */
  def ||!!(p: String)(implicit f: FoldLeft[IN]) = this.||(p) ⟴ !!(p)

  /**
   * Returns all occurrences of a given request parameter in the request body if it exists or in the request URI
   * otherwise.
   */
  def ^||!!^(implicit f: FoldLeft[IN]) : Kleisli[Option, String, NonEmptyList[List[Char]]] =
    kleisli((p : String) => {(this ||!! p).nel })

  /**
   * The request method of the status line.
   */
  def method = line.method

  /**
   * The request URI of the status line.
   */
  def uri = line.uri

  /**
   * The request version of the status line.
   */
  def version = line.version

  /**
   * The request path of the request URI of the status line.
   */
  def path = line.uri.path

  /**
   * Returns the path extension - characters after the last dot (.) in the path.
   */
  def pathExtension = line.uri.pathExtension

  /**
   * The query string of the request URI of the status line.
   */
  def queryString = line.uri.queryString

  /**
   * The request version major of the status line.
   */
  def versionMajor = line.version.major

  /**
   * The request version minor of the status line.
   */
  def versionMinor = line.version.minor


  /**
   * Returns <code>true</code> if the request path of the request URI satisfies the given condition.
   */
  def path(f: NonEmptyList[Char] => Boolean) = f(line.uri.path)

  /**
   * Returns <code>true</code> if the query string of the request URI satisfies the given condition.
   */
  def queryString(f: List[Char] => Boolean) = line.uri.queryString exists f

  /**
   * Returns <code>true</code> if the request path of the request URI equals the given value.
   */
  def pathEquals(s: String) = path(_.mkString == s)

  /**
   * Returns <code>true</code> if the request path starts with the given value.
   */
  def pathStartsWith(s: String) = path.mkString startsWith s 

  /**
   * Returns <code>true</code> if the query string of the request URI equals the given value. 
   */
  def queryStringEquals(s: String) = queryString(_.mkString == s)

  /**
   * Returns <code>true</code> if this request method is OPTIONS.
   */
  def isOptions = line.method == OPTIONS

  /**
   * Returns <code>true</code> if this request method is GET.
   */
  def isGet = line.method == GET

  /**
   * Returns <code>true</code> if this request method is HEAD.
   */
  def isHead = line.method == HEAD

  /**
   * Returns <code>true</code> if this request method is POST.
   */
  def isPost = line.method == POST

  /**
   * Returns <code>true</code> if this request method is PUT.
   */
  def isPut = line.method == PUT

  /**
   * Returns <code>true</code> if this request method is DELETE.
   */
  def isDelete = line.method == DELETE

  /**
   * Returns <code>true</code> if this request method is TRACE.
   */
  def isTrace = line.method == TRACE

  /**
   * Inspects the user-agent header to determine if the request was made by Microsoft Internet Explorer.
   */
  def isInternetExplorer = this(UserAgent).mkString.toLowerCase contains "msie" 

  import scalaz.http.response._

  trait Debug[OUT[_]] {
    def apply[A](f: IN[Byte] => A)(implicit e: Empty[OUT], b: Body[OUT, xml.Elem], s: Semigroup[OUT[Byte]]): Response[OUT]
  }

  /**
   * Create a response that details the parts of the request in a XHTML document. 
   */
  def debug[OUT[_]] = new Debug[OUT] {
    def apply[A](f: IN[Byte] => A)(implicit e: Empty[OUT], b: Body[OUT, xml.Elem], s: Semigroup[OUT[Byte]]) = {
      implicit val request = Request.this
      Response.emptyHeadersBodyResponse[OUT](StatusLine.statusLine[IN](OK)).xhtml <<
        <html xmlns="http://www.w3.org/1999/xhtml">
          <head>
            <title>Request Details</title>
          </head>
          <body>
            <div>
              {
                List(("Method", method),
                     ("URI Path", uri.path.mkString),
                     ("URI Query String", uri.queryString map (_.mkString) getOrElse <i>N/A</i>),
                     ("Version (major)", versionMajor.toLong),
                     ("Version (minor)", versionMinor.toLong)) map {
                  case (k, v) =>
                    <div>{ k }</div>
                    <h4>{ v }</h4>
                }
              }
            </div>
            <hr/>
            <div>
              {
                if(headers.isEmpty)
                  <i>N/A</i>
                else
                  <ul>
                  {
                    headers map {
                      case (h, v) => <li><div><strong>{ h.asString }</strong><br/>{ v.mkString }</div></li>
                    }
                  }
                  </ul>
              }
            </div>
            <hr/>
            <div>
            {
              f(body)
            }
            </div>
          </body>
        </html>      
    }
  }
}

/**
 * HTTP request.
 * <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5">RFC 2616 Section 5 Request</a>.
 */
object Request {
  /**
   * Construct a request with the given status line, headers and body.
   */
  def request[IN[_]](l: Line, h: List[(RequestHeader, NonEmptyList[Char])], b: IN[Byte]) = new Request[IN] {
    val line = l
    val headers = h
    val body = b
  }

  object MethodPath {
    def unapply[IN[_]](r: Request[IN]): Option[(Method, String)] =
      Some(r.line.method, r.line.uri.path.toList.mkString)
  }

  object MethodUri {
    def unapply[IN[_]](r: Request[IN]): Option[(Method, Uri)] =
      Some(r.line.method, r.line.uri)
  }

  object Path {
    def unapply[IN[_]](r: Request[IN]): Option[(String)] =
      Some(r.line.uri.path.toList.mkString)
  }

  object Uri {
    def unapply[IN[_]](r: Request[IN]): Option[Uri] =
      Some(r.line.uri)
  }

  object Method {
    def unapply[IN[_]](r: Request[IN]): Option[Method] =
      Some(r.line.method)
  }

  object Version {
    def unapply[IN[_]](r: Request[IN]): Option[Version] =
      Some(r.line.version)
  }

  /**
   * Workarounds to <a href="http://lampsvn.epfl.ch/trac/scala/ticket/1227">Ticket 1227</a> stream-body requests.
   */
  object Stream {
    import scala.Stream

    /**
     * Extracts the given request into request method and request path.
     */
    object MethodPath {
      def unapply(r: Request[Stream]): Option[(Method, String)] =
        Some(r.line.method, r.line.uri.path.toList.mkString)
    }

    /**
     * Extracts the given request into request method and request URI.
     */
    object MethodUri {
      def unapply(r: Request[Stream]): Option[(Method, Uri)] =
        Some(r.line.method, r.line.uri)
    }

    /**
     * Extracts the given request into request path.
     */
    object Path {
      def unapply(r: Request[Stream]): Option[(String)] =
        Some(r.line.uri.path.toList.mkString)
    }

    /**
     * Extracts the given request into request URI.
     */
    object Uri {
      def unapply(r: Request[Stream]): Option[Uri] =
        Some(r.line.uri)
    }

    /**
     * Extracts the given request into request method.
     */
    object Method {
      def unapply(r: Request[Stream]): Option[Method] =
        Some(r.line.method)
    }

    /**
     * Extracts the given request into request version.
     */
    object Version {
      def unapply(r: Request[Stream]): Option[Version] =
        Some(r.line.version)
    }
  }
}
