package scalaz
package syntax

package object std {
  object booleanV extends ToBooleanV
  object intV extends ToIntV
  object listV extends ToListV
  object streamV extends ToStreamV
  object function2V extends ToFunction2V
  object allV extends ToAllStdV
  object stringV extends ToStringV
  object optionV extends ToOptionV with ToOptionIdV
}
