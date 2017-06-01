package scalaz.example

object NewTypeUsage extends App {

  import scalaz._
  import std.anyVal.{booleanInstance, booleanDisjunctionNewTypeInstance, booleanConjunctionNewTypeInstance}
  import Tags.{Conjunction, Disjunction}
  import std.list.listInstance

  assert(!booleanInstance.disjunction.zero)
  assert(booleanInstance.conjunction.zero)

  assert(!Tag.unwrap(Monoid[Boolean @@ Disjunction].zero))
  assert(Tag.unwrap(Monoid[Boolean @@ Conjunction].zero))

  val bools = List(true, false)

  val boolDisjunctions: List[Boolean @@ Disjunction] = Tag.subst(bools)
  assert(Tag.unwrap(listInstance.fold(boolDisjunctions)))

  val boolConjunctions: List[Boolean @@ Conjunction] = Tag.subst(bools)
  assert(!Tag.unwrap(listInstance.fold(boolConjunctions)))
}
