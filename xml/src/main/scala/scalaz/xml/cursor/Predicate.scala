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
    def show(p: Predicate[A]) =
      ("Predicate{" + (p.name match {
        case None => "<no name>}"
        case Some(n) => "name=" + n.mkString + "}"
      })).toList
  }

  implicit def PreciateEqual[A]: Equal[Predicate[A]] =
    Equal.equalBy(_.name)
}

object Predicate extends Predicates {

  import Lens._
  import CoStateT._

  def predPredicateL[A]: Predicate[A] @-@ (A => Boolean) =
    lens(x => coState(b => predicate(b, x.name), x.pred))

  def namePredicateL[A]: Predicate[A] @-@ Option[List[Char]] =
    lens(x => coState(b => predicate(x.pred, b), x.name))

}
