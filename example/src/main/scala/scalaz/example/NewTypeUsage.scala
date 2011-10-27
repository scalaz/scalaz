package scalaz.example

object NewTypeUsage extends App {

  import scalaz._
  import std.anyVal.{Conjunction, Disjunction, booleanInstance, booleanDisjunctionNewTypeInstance, booleanConjunctionNewTypeInstance}
  import std.list.listInstance

  assert(!booleanInstance.disjunction.zero)
  assert(booleanInstance.conjunction.zero)

  assert(!Monoid[Boolean @@ Disjunction].zero)
  assert(Monoid[Boolean @@ Conjunction].zero)

  val bools = List(true, false)

  val boolDisjunctions: List[Boolean @@ Disjunction] = Tag.subst(bools)
  assert(listInstance.foldMapIdentity(boolDisjunctions))
  
  val boolConjunctions: List[Boolean @@ Conjunction] = Tag.subst(bools)
  assert(!listInstance.foldMapIdentity(boolConjunctions))
}
