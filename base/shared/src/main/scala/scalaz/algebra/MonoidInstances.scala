package scalaz
package algebra

trait MonoidInstances {
  implicit val stringMonoid: Monoid[String] = instanceOf(new MonoidClass[String] {
    def append(a1: String, a2: => String) = a1 + a2
    def empty                             = ""
  })
  implicit val unitMonoid: Monoid[Unit] = instanceOf(new MonoidClass[Unit] {
    def append(a1: Unit, a2: => Unit) = ()
    def empty                         = ()
  })
}
