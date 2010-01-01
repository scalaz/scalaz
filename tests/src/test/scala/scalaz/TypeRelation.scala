package scalaz

// Very similar implementations of these bounds (with one colon rather than two) are in trunk Predef.
// todo Cut over to use them instead once they stabilise.

/**
* Type Equivalence Bound
*/
sealed abstract class =::=[From, To] extends (From => To) {
  override def toString = "=::=[From, To]"
}

object =::= {
  implicit def typeEquals[A]: A =::= A = new (A =::= A) {
    def apply(x: A) = x
  }
}

/**
* Type Conformance Bound
*/
sealed abstract class <::<[-From, +To] extends (From => To)

object <::< {
  implicit def conforms[A]: A <::< A = new (A <::< A) {
    def apply(x: A) = x
  }
}

/**
* Type View Bound
*/
sealed abstract class <%%<[-From, +To] {
  def view(f: From): To
}

object <%%< {
  implicit def viewableAs[To, From <% To]: <%%<[From, To] = new (From <%%< To) {
    def view(x: From): To = x
  }
}

trait TypeRelationLowPriority {
  implicit def defaultTypeEquals[From, To]: Option[From =::= To] = None

  implicit def defaultConformance[From, To]: Option[From <::< To] = None

  implicit def defaultView[From, To]: Option[From <%%< To] = None
}

object TypeRelation extends TypeRelationLowPriority {
  implicit def typeEqualsOption[A](implicit check: A =::= A): Option[A =::= A] = Some(check)

  implicit def conformsOption[A](implicit check: A <::< A): Option[A <::< A] = Some(check)

  implicit def viewableAsOption[To, From <% To]: Option[From <%%< To] = Some(<%%<.viewableAs[To, From])

  def typeEquals[From, To](implicit check: Option[From =::= To]) = check.isDefined

  def conforms[From, To](implicit check: Option[From <::< To]) = check.isDefined

  def viewableAs[From, To](implicit check: Option[From <%%< To]) = check.isDefined
}
