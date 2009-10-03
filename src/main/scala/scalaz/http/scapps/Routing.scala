package scalaz.http.scapps

import scalaz.http.request._
import scalaz.http.response.Response
import scalaz.NonEmptyList, NonEmptyList._
import scalaz.Scalaz._

// Methods to get route parsing functions
object Routing {
  // returns a new request with the prefix stripped if it matches, else none
  def startsWith(prefix: String): (Request[Stream] => Option[Request[Stream]])= {
    def f(prefix: String)(r: Request[Stream]): Option[Request[Stream]] = {
      (r.pathStartsWith(prefix)).option(
        r(r.uri(stringToNel(r.path.list.mkString.replaceFirst(prefix, ""), NonEmptyList.nel('/'))))
      )
    }
    f(prefix) _
  }

  // returns a new request with the uri stripped to '/', if it matches exactly, else none
  def exactPath(path: String): (Request[Stream] => Option[Request[Stream]]) = {
    def g(s: String)(r: Request[Stream]): Option[Request[Stream]] = (r.pathEquals(s)).option(r(r.uri(NonEmptyList.nel('/'))))
    g(path) _
  }

  def withParam(key: String) = (r: Request[Stream]) => (r !? key).option(r)

  def methodM(m: Method) : (Request[Stream] => Option[Request[Stream]]) = (r: Request[Stream]) => { (r.method.equals(m)).option(r) }

  private def stringToNel(s : String, n : NonEmptyList[Char]) = {
    if (s.length == 0) n else {
      val l = s.toList
      nel(l.head, l.tail)
    }
  }

  // interprets the value of _method in a post as an HTTP method
  def methodHax = (r: Request[Stream]) => {
    import scalaz.http.request.Method._
    val mbMeth: Option[Method] = (r | "_method") >>= (_.mkString : Option[Method])
    Some((mbMeth |> (r(_))) getOrElse (r))
  }
}

