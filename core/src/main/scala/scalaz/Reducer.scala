package scalaz

import Scalaz._
/**
 * A Reducer[C,M] is a Monoid[M] that maps
 * values of type C through `unit` to values of type M. A C-Reducer may also
 * supply operations which tack on another C to an existing 'Monoid' M on the left
 * or right. These specialized reductions may be more efficient in some scenarios
 * and are used when appropriate by a 'Generator'. The names 'cons' and 'snoc' work
 * by analogy to the synonymous operations in the list monoid.
 * 
 * Minimal definition: 'unit' or 'snoc'
 * 
 * Based on a Haskell library by Edward Kmett
 **/
abstract class Reducer[C, M](implicit m: Monoid[M]) {
  val monoid = m
  def unit(c: C): M = snoc(mzero, c)
  def snoc(m: M, c: C): M = m |+| unit(c)
  def cons(c: C, m: M): M = unit(c) |+| m
}

trait Reducers {
  import Scalaz._
  implicit def ReducerMonoid[C, M](r: Reducer[C, M]) = r.monoid

  implicit def ReducerTuple2[C, M, N](implicit m: Reducer[C, M], n: Reducer[C, N]) = {
    implicit val mm = m.monoid
    implicit val mn = n.monoid
    new Reducer[C, (M, N)] {
      override def unit(x: C) = (x.unit[M], x.unit[N])
      override def snoc(p: (M,N), x: C) = (p._1 snoc x, p._2 snoc x)
      override def cons(x: C, p: (M,N)) = (x cons p._1, x cons p._2)
    }
  }
}
