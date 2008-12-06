package fjs.data

import fj.data.Array.array

object Array {
  implicit def ScalaArray_Array[A](a: scala.Array[A]) = array(a: _*)

  implicit def Array_ScalaArray[A](a: fj.data.Array[A]) = a.array
}
