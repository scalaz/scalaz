package scalaz

import Scalaz._
/**
 * A Reducer[C,M] is a Monoid[M] that maps
 * values of type C through `unit` to values of type M. A C-Reducer may also
 * supply operations which tack-on another C to an existing 'Monoid' M on the left
 * or right. These specialized reductions may be more efficient in some scenarios
 * and are used when appropriate by a 'Generator'. The names 'cons' and 'snoc' work
 * by analogy to the synonymous operations in the list monoid.
 * 
 * Minimal definition: 'unit' or 'snoc'
 * 
 * Based on a Haskell library by Edward Kmett
 **/
abstract class Reducer[C, M](implicit mn: Monoid[M]) {
  val monoid = mn
  def unit(c: C): M = snoc(mzero, c)
  def snoc(m: M, c: C): M = m |+| unit(c)
  def cons(c: C, m: M): M = unit(c) |+| m
}

trait Reducers {
  implicit def ReducerMonoid[C, M](c: Reducer[C, M]): Monoid[M] = c.monoid


}
