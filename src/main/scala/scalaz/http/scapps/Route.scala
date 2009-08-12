package scalaz.http.scapps

import scalaz.Kleisli
import scalaz.{OptionW, StringW}
import scalaz.http.request._
import scalaz.http.response.Response
import scalaz.NonEmptyList._
import scalaz.NonEmptyList
import scalaz.Scalaz._

object OptionKleisli {
  // Scalaz doesn't supply this. Yet.
  implicit def OptionKleisli[A, B](f: A => Option[B]): Kleisli[Option, A, B] = Kleisli.kleisli[Option](f)
}

object Route {
  def firstSome[A, B](fs: List[Kleisli[Option, A, B]])(a: A): Option[B] = (fs.elements.map(_(a)).find(_.isDefined)).join

  implicit def ListKleisliKleisli[A, B](fs: List[Kleisli[Option, A, B]]): Kleisli[Option, A, B] = Kleisli.kleisli[Option](firstSome(fs) _)

  implicit def PathKliesli(s: String) = Kleisli.kleisli[Option](exactdir(s))

  implicit def MethodKliesli(m: Method) = Kleisli.kleisli[Option](methodM(m))

  implicit def PathMethodKliesli(t: Tuple2[Method, String]) = MethodKliesli(t._1) >=> PathKliesli(t._2)

  def stringToNel(s : String, n : NonEmptyList[Char]) = {
    if (s.length == 0) n else {
      val l = s.toList
      nel(l.head, l.tail)
    }
  }
  // returns a new request with the prefix stripped if it matches, else none
  def dir(prefix: String) = {
    def f(prefix: String)(r: Request[Stream]): Option[Request[Stream]] = {
      (r.pathStartsWith(prefix)).option(
        r(r.uri(stringToNel(r.path.list.mkString.replaceFirst(prefix, ""), NonEmptyList.nel('/'))))
      )
    }
    f(prefix) _
  }


  // returns a new request with the uri stripped to '/', if it matches exactly, else none
  def exactdir(s: String) : (Request[Stream] => Option[Request[Stream]]) = {
    def g(s: String)(r: Request[Stream]): Option[Request[Stream]] = (r.pathEquals(s)).option(r(r.uri(NonEmptyList.nel('/'))))
    g(s) _
  }

  def withParam(key: String) = (r: Request[Stream]) => (r !? key).option(r)

  def methodM(m: Method) : (Request[Stream] => Option[Request[Stream]]) = (r: Request[Stream]) => { (r.method.equals(m)).option(r) }

  // interprets the value of _method in a post as an HTTP method
  def methodHax = (r: Request[Stream]) => {
    import scalaz.http.request.Method._
    val mbMeth: Option[Method] = (r | "_method") >>= (_.mkString : Option[Method])
    Some((mbMeth |> (r(_))) getOrElse (r))
  }
}


trait Wat[B] {
  import Route._
  import OptionKleisli._

  val k : Kleisli[Option, Request[Stream], B]

  def !:(m : Method) = methodM(m) >=> k

  def /:(s : String) = dir(s) >=> k
}

object Wat {
  import OptionKleisli._

  implicit def KleisiToWat[B](kl : Kleisli[Option, Request[Stream], B]) = new Wat[B] {
    val k = kl
  }

  implicit def FunctionToWat[B](f : (Request[Stream] => Option[B])) = new Wat[B] {
    val k = (f : Kleisli[Option, Request[Stream], B])
  }

  def main {
    "/api/beers" /: (GET !: ((_ : Request[Stream]) => Some(5)))
  }
}
