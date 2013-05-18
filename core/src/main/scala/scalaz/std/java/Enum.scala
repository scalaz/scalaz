package scalaz
package std.java

trait EnumInstances {
  implicit def enumInstance[E <: java.lang.Enum[E]] = Equal.equal[E](_ eq _)
}

object enum extends EnumInstances
