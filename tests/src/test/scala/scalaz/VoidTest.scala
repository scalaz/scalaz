package scalaz

import scalaz.Scalaz._

object VoidTest {

  def _void(v: Void): Unit = {
    v.absurd[Int]
    v.absurd[Nothing]
    v.absurd : Int
    v.absurd : Nothing
  }
}
