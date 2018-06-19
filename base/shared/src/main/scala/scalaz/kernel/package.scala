package scalaz

import scala.inline

package object kernel {

  type InstanceOf[T] = InstanceOfModule.impl.InstanceOf[T]

  @inline
  private[scalaz] final def instanceOf[T]: T => InstanceOf[T] =
    InstanceOfModule.impl.instanceOf
}
