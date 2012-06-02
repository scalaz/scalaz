package scalaz
package syntax

package object std {
  object boolean extends ToBooleanOps
  object list extends ToListOps
  object stream extends ToStreamOps
  object vector extends ToVectorOps
  object function1 extends ToFunction1Ops
  object function2 extends ToFunction2Ops
  object string extends ToStringOps
  object option extends ToOptionOps with ToOptionIdOps

  object all extends ToAllStdOps
}
