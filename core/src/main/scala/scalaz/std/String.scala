package scalaz
package std

trait StringInstances {
  implicit object stringInstance extends Monoid[String] with Show[String] with Equal[String] with Order[String]{
    def append(f1: String, f2: => String): String = f1 + f2
    def zero: String = ""
    def show(f: String): List[Char] = f.toList
    def order(x: String, y: String): Ordering = Ordering.fromInt(x.compareTo(y))
  }
}

object string extends StringInstances