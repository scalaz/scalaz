package scalaz
package typeclass

trait MonoidInstances {

  implicit val stringMonoid: Monoid[String] = instanceOf(new MonoidClass[String] {
    def append(a1: String, a2: => String) = a1 + a2
    def empty                             = ""
  })

}
