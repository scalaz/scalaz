package scalaz
package laws

import tc._

object HomomorphismLaws {
  // To be a monoid homomorphism, a function must satisfy both `monoidZero`
  // and `semigroupAppend`.
  def monoidZero[A, B, T]
    (f: A => B)
    (assert: (B, B) => T)
    (implicit A: Monoid[A], B: Monoid[B]): T = {
    assert(f(A.mzero), B.mzero),
  }

  def semigroupAppend[A, B, T]
    (fst: A, snd: A)
    (f: A => B)
    (assert: (B, B) => T)
    (implicit A: Semigroup[A], B: Semigroup[B]): T = {
    assert(B.mappend(f(fst), f(snd)), f(A.mappend(fst, snd)))
  }

  // all natural transformations are Functor homomorphisms,
  // Contravariant homomorphisms,
  // InvariantFunctor homomorphisms,
  // and Phantom homomorphisms.

  // all binatural transformations are Bifunctor homomorphisms
  // and profunctor homomorphisms.


  def applyAp
  def applicativeIdentity
  def bindFlatMap
  def monadIdentity
  def cobindCoflatMap
  def comonadIdentity
  def categoryIdentity
  def semicategoryCompose
  def strongFirst
  def choiceLeft

}
