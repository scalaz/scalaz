package scalaz
package data

import tc._

sealed abstract class EndoModule {
  type Endo[=>:[_, _], A]

  def apply[=>:[_, _], A](f: A =>: A): Endo[=>:, A]
  def run[=>:[_, _], A](f: Endo[=>:, A]): A =>: A

  def endoSemigroup[=>:[_, _]: Semicategory, A]: Semigroup[Endo[=>:, A]]
  def endoMonoid[=>:[_, _]: Category, A]: Monoid[Endo[=>:, A]]
}

object EndoModule {
  implicit def endoSemigroup[=>:[_, _]: Semicategory, A]: Semigroup[Endo[=>:, A]] =
    Endo.endoSemigroup[=>:, A]
  implicit def endoMonoid[=>:[_, _]: Category, A]: Monoid[Endo[=>:, A]] =
    Endo.endoMonoid[=>:, A]
}

private[data] object EndoImpl extends EndoModule {
  type Endo[=>:[_, _], A] = A =>: A

  def apply[=>:[_, _], A](f: A =>: A): Endo[=>:, A] = f
  def run[=>:[_, _], A](f: Endo[=>:, A]): A =>: A   = f

  def endoMonoid[=>:[_, _], A](implicit F: Category[=>:]): Monoid[Endo[=>:, A]] =
    instanceOf(new MonoidClass[Endo[=>:, A]] with EndoSemigroup[=>:, A] {
      val F0: Semicategory[=>:] = Scalaz.categorySemicategory[=>:](F)
      def mempty: Endo[=>:, A]  = apply(F.id)
    })

  def endoSemigroup[=>:[_, _], A](implicit F: Semicategory[=>:]): Semigroup[Endo[=>:, A]] =
    instanceOf(new EndoSemigroup[=>:, A] {
      val F0 = F
    })

  private trait EndoSemigroup[=>:[_, _], A] extends SemigroupClass[Endo[=>:, A]] {
    val F0: Semicategory[=>:]

    def mappend(a1: Endo[=>:, A], a2: Endo[=>:, A]): Endo[=>:, A] =
      F0.compose(a1, a2)
  }
}
