package scalaz.example

object NewTypeUsage extends App {

  import scalaz._
  import std.AnyVal.{Conjunction, Disjunction, boolean, booleanConjunctionNewType}
  import std.List.list

  assert(boolean.disjunction.zero)
  assert(!boolean.conjunction.zero)

  assert(!Monoid[Boolean ## Disjunction].zero)
  assert(Monoid[Boolean ## Conjunction].zero)

  val bools = List(true, false)

  val boolDisjunctions: List[Boolean ## Conjunction] = NewType.subst[Boolean, List, Disjunction](bools)
  assert(list.foldMapIdentity(boolDisjunctions))

  val boolConjunctions: List[Boolean ## Conjunction] = NewType.subst[Boolean, List, Conjunction](bools)
  assert(!list.foldMapIdentity(boolConjunctions))
}
