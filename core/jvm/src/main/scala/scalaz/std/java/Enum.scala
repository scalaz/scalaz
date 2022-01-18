package scalaz
package std.java

import scala.reflect.ClassTag

trait EnumInstances {
  implicit def enumInstance[E <: java.lang.Enum[E]]: Equal[E] = Equal.equal[E](_ eq _)
}

object `enum` extends EnumInstances {
  implicit def enumInstance2[E <: java.lang.Enum[E] : ClassTag]: Enum[E] = new Enum[E] {
    private[this] lazy val values: Array[E] = implicitly[ClassTag[E]].runtimeClass.asInstanceOf[Class[E]].getEnumConstants

    override def equal(x: E, y: E): Boolean = x eq y
    override def equalIsNatural: Boolean = true

    override def pred(e: E): E  = values((values.indexOf(e) + values.length - 1) % values.length)
    override def succ(e: E): E  = values((values.indexOf(e) + 1) % values.length)
    override def min: Option[E] = values.headOption
    override def max: Option[E] = values.lastOption
    override def order(x: E, y: E): Ordering =
      Ordering.fromInt(values.indexOf(x) - values.indexOf(y))
  }
}
