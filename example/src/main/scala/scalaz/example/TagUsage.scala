package scalaz
package example

/**
  scalaz contains a way to simulate something similar to a Haskell
  newtype, where we can take an existing type, and create a new type
  from it, and allow us to create new typeclass instances for our
  newly created type to get different behaviors.  The same thing could
  be done with scala 2.10's Value Classes:
  http://docs.scala-lang.org/overviews/core/value-classes.html however
  one has to be very careful when using value classes, because there
  are a lot of instances in which using a value class will incur a
  runtime boxing/unboxing of your value, which incurs a runtime
  cost. The scalaz tagged types will never cause boxing of a value.
  */
object TagUsage extends App {
  import Tags._
  import syntax.monoid._
  import syntax.foldable._
  import syntax.equal._
  import std.anyVal._
  import std.option._
  import std.list._

  // Int has a Monoid by default which performs addition
  assert((3 |+| 3) === 6)
  assert(Monoid[Int].zero === 0)

  // Though the set of Ints forms a perfectly valid Monoid using
  // multiplication as the binary operation. We can Tag Ints with the
  // Multiplication tag, and this monoid will be selected.
  assert((Multiplication(3) |+| Multiplication(3)) === Multiplication(9))
  assert(Monoid[Int @@ Multiplication].zero === Multiplication(1))

  // There is no default Monoid for booleans, but there are Monoids
  // for booleans tagged with either the Conjunction or Disjunction
  // tag.
  assert((Conjunction(true) |+| Conjunction(true)) === Conjunction(true))
  assert((Conjunction(true) |+| Conjunction(false)) === Conjunction(false))
  assert((Conjunction(false) |+| Conjunction(false)) === Conjunction(false))
  assert(Monoid[Boolean @@ Conjunction].zero === Conjunction(true))

  assert((Disjunction(true) |+| Disjunction(true)) === Disjunction(true))
  assert((Disjunction(true) |+| Disjunction(false)) === Disjunction(true))
  assert((Disjunction(false) |+| Disjunction(false)) === Disjunction(false))
  assert(Monoid[Boolean @@ Disjunction].zero === Disjunction(false))

  // tags have a convenience method name subst for tagging an F[A] as
  // a F[A @@ SomeTag]
  assert(Conjunction.subst(List(false, true, false)).suml === Conjunction(false))
  assert(Conjunction.subst(List.empty[Boolean]).suml === Conjunction(true))
  assert(Conjunction.subst(List(true, true)).suml === Conjunction(true))

  assert(Disjunction.subst(List(false, true, false)).suml === Disjunction(true))
  assert(Disjunction.subst(List.empty[Boolean]).suml === Disjunction(false))
  assert(Disjunction.subst(List(false, false)).suml === Disjunction(false))

  // tagging a value does not actually alter the value, it is merely
  // typecasting the value to a tagged type, all this is actually lost
  // at runtime, so when we do a non-typesafe equal comparision, it
  // actually works.
  @unchecked def equalsTrue(x: Any): Boolean = x == true
  assert(equalsTrue(Disjunction(true)))

  // however at compile time, they appear to be different types, so a
  // typesafe comparison would fail to compile:
  // assert(Disjunction(true) === true)

  // Tags have an unwrap method which converts the value back to a untagged type:
  assert(Disjunction.unwrap(Disjunction(true)) === true)
  assert(Conjunction.unwrap(Conjunction.subst(List(false, true, false)).suml) === false)

  // Some other Tags in scalaz:

  // A Semigroup that selects the minimum element:
  assert(MinVal.unwrap(MinVal(3) |+| MinVal(1) |+| MinVal(5)) === 1)

  // A Semigroup that selects the maximum element:
  assert(MaxVal.unwrap(MaxVal(3) |+| MaxVal(1) |+| MaxVal(5)) === 5)

  // A Semigroup that selects the first element:
  assert(FirstVal.unwrap(FirstVal(1) |+| FirstVal(5)) === 1)

  // A Semigroup that selects the last element:
  assert(LastVal.unwrap(LastVal(1) |+| LastVal(5)) === 5)

  // Tags are not only useful for selecting typeclass instances
  // lets create our own tag, named Sorted which indicates a List that has been sorted
  sealed trait Sorted
  val Sorted = Tag.of[Sorted]

  // a sort function which will sort then add the Tag
  def sortList[A: scala.math.Ordering](as: List[A]): List[A] @@ Sorted =
    Sorted(as.sorted)

  // now we can define a function which takes lists which are tagged as being sorted
  def minOption[A](a: List[A] @@ Sorted): Option[A] = Sorted.unwrap(a).headOption

  implicit val ord = implicitly[Order[Option[Int]]].toScalaOrdering
  assert(minOption(sortList(List(3,2,1,5,3))) === Some(1))

  // we can also use pattern matching:
  def minOption_v2[A]: List[A] @@ Sorted ⇒ Option[A] = {
    case Sorted(list) ⇒ list.headOption
  }
}
