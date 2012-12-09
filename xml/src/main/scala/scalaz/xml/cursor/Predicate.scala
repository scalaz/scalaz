package scalaz
package xml
package cursor

sealed trait Predicate[A] {
  val name: Option[List[Char]]

  import Predicate._

  val pred: A => Boolean

  def apply(a: A): Boolean =
    pred(a)

  def not(a: A) =
    !apply(a)

  def nameOr(s: => List[Char]): List[Char] =
    name getOrElse s

  def hasName: Boolean =
    name.isDefined

  def hasNoName: Boolean =
    !hasName

  def contramap[B](f: B => A): Predicate[B] =
    predicate(b => apply(f(b)), name)
}

trait Predicates {
  type CPredicate =
  Predicate[Cursor]

  def predicate[A](p: A => Boolean, n: Option[List[Char]] = None): Predicate[A] =
    new Predicate[A] {
      val pred = p
      val name = n
    }

  def npredicate[A](p: A => Boolean, n: String): Predicate[A] =
    predicate(p, Some(n.toList))

  def truePredicate[A]: Predicate[A] =
    npredicate(_ => true, "true")

  def falsePredicate[A]: Predicate[A] =
    npredicate(_ => false, "false")

  import std.AllInstances._

  implicit def PredicateShow[A]: Show[Predicate[A]] = new Show[Predicate[A]] {
    override def shows(p: Predicate[A]) =
      ("Predicate{" + (p.name match {
        case None => "<no name>}"
        case Some(n) => "name=" + n.mkString + "}"
      }))
  }

  implicit def PreciateEqual[A]: Equal[Predicate[A]] =
    Equal.equalBy(_.name)
}

object Predicate extends Predicates {

  import Lens._
  import StoreT._

  def predPredicateL[A1, A2]: LensFamily[Predicate[A1], Predicate[A2], A1 => Boolean, A2 => Boolean] =
    lensFamily(x => indexedStore(x.pred)(b => predicate(b, x.name)))

  def namePredicateL[A]: Predicate[A] @> Option[List[Char]] =
    lens(x => store(x.name)(b => predicate(x.pred, b)))

}
