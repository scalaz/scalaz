package scalaz
package syntax

package object std {
  object booleanV extends ToBooleanOps
  object listV extends ToListOps
  object streamV extends ToStreamOps
  object function2V extends ToFunction2Ops
  object allV extends ToAllStdOps
  object stringV extends ToStringOps
  object optionV extends ToOptionOps with ToOptionIdOps
}
