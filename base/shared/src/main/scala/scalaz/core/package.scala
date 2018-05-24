package scalaz

package object core {

  val Void: VoidModule = VoidImpl

}

package core {
  trait CoreFunctions
    extends scala.AnyRef
      with EqFunctions
      with VoidFunctions
}