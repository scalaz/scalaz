package scalaz

sealed abstract class InstanceOfModule {
  type InstanceOf[T] <: T
  def instanceOf[T](t: T): InstanceOf[T]
}

object InstanceOfModule {
  val impl: InstanceOfModule = new InstanceOfModule {
    override type InstanceOf[T] = T
    override def instanceOf[T](t: T) = t
  }
}
