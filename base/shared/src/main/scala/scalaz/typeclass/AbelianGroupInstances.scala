package scalaz
package typeclass

trait AbelianGroupInstances {

  implicit val int: AbelianGroup[Int] = new AbelianGroupClass[Int] {
    def append(a1: Int, a2: => Int) = a1 + a2
    def empty = 0
    def inverse(a:Int) = a * -1
  }

}
