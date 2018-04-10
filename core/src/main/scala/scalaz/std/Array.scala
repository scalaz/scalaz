package scalaz
package std

trait ArrayInstances {
  implicit def arrayShow[A](implicit SA: Show[A]): Show[Array[A]] = new Show[Array[A]] {
    override def shows(a: Array[A]): String = a.map(SA.shows).mkString("[", ",", "]")
  }
}

object array extends ArrayInstances
