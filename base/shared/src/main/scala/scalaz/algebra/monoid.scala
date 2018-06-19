package scalaz
package algebra

import kernel.instanceOf

trait MonoidClass[A] extends SemigroupClass[A] {
  def mempty: A
}

trait MonoidInstances {
  implicit val stringMonoid: Monoid[String] = instanceOf(new MonoidClass[String] {
    def mappend(a1: String, a2: => String) = a1 + a2
    def mempty                             = ""
  })
  implicit val unitMonoid: Monoid[Unit] = instanceOf(new MonoidClass[Unit] {
    def mappend(a1: Unit, a2: => Unit) = ()
    def mempty                         = ()
  })
}
