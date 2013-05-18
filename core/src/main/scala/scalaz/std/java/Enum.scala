package scalaz
package std.java

trait EnumInstances {
    
   implicit def EnumEqual[E <: java.lang.Enum[E]](implicit ev: E <:< java.lang.Enum[E]) = Equal.equal[E]( _ eq _ )
   
}

object enum extends EnumInstances