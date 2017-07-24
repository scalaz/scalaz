package scalaz

object ApTest extends SpecLite {
  object instances {
    def semigroup[A: Monoid] = Semigroup[Ap[List, A]]
    def monoid[A: Monoid] = Monoid[Ap[Maybe, A]]
  }
}
