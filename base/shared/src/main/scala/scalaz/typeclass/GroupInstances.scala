package scalaz
package typeclass

trait GroupInstances {

  implicit val int: Group[Int] = new GroupClass[Int] {
    def append(a1: Int, a2: => Int) = a1 + a2
    def empty = 0
    def inverse(a:Int) = a * -1
  }

}
