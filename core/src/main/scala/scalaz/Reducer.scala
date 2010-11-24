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
abstract class Reducer[C, M](implicit mm: Monoid[M]) {
  val monoid = mm
  def unit(c: C): M = snoc(mzero, c)
  def snoc(m: M, c: C): M = m |+| unit(c)
  def cons(c: C, m: M): M = unit(c) |+| m

  final def &&&[N](r: Reducer[C, N]): Reducer[C, (M, N)] = {
    implicit val n = r.monoid
    new Reducer[C, (M, N)] {
      override def unit(x: C) = (Reducer.this.unit(x), r.unit(x))
      override def snoc(p: (M, N), x: C) = (Reducer.this.snoc(p._1, x), r.snoc(p._2, x))
      override def cons(x: C, p: (M, N)) = (Reducer.this.cons(x, p._1), r.cons(x, p._2))
    }
  }
}

trait Reducers {
  import Scalaz._
  implicit def ReducerMonoid[C, M](r: Reducer[C, M]) = r.monoid

  /** Construct a Reducer with the given unit function and monoid **/
  def Reducer[C, M: Monoid](unit: C => M) = {
    val u = unit
    new Reducer[C, M] {
      override def unit(c: C) = u(c)
    }
  }

  def Reducer[M: Monoid]: Reducer[M, M] = Reducer(x => x)

  def ListReducer[C] = new Reducer[C, List[C]] {
    override def unit(c: C) = List(c)
    override def cons(c: C, cs: List[C]) = c :: cs
  }

  def StreamReducer[C] = new Reducer[C, Stream[C]] {
    override def unit(c: C) = Stream(c)
    override def cons(c: C, cs: Stream[C]) = c #:: cs
  }

  def UnitReducer[C]: Reducer[C, Unit] = Reducer((c: C) => ())

  def AnyReducer: Reducer[Boolean, Boolean] = Reducer(x => x)

  def AllReducer: Reducer[Boolean, BooleanConjunction] = Reducer(x => conjunction(x))

  def EndoReducer[A]: Reducer[A => A, Endo[A]] = Reducer(EndoTo(_))

  def DualReducer[A: Monoid]: Reducer[A, Dual[A]] = Reducer(_.dual)

  def IntProductReducer: Reducer[Int, IntMultiplication] = Reducer(x => multiplication(x))

  def CharProductReducer: Reducer[Char, CharMultiplication] = Reducer(x => multiplication(x))

  def ByteProductReducer: Reducer[Byte, ByteMultiplication] = Reducer(x => multiplication(x))

  def LongProductReducer: Reducer[Long, LongMultiplication] = Reducer(x => multiplication(x))

  def ShortProductReducer: Reducer[Short, ShortMultiplication] = Reducer(x => multiplication(x))

  def BigIntProductReducer: Reducer[BigInt, BigIntMultiplication] = Reducer((x: BigInt) => multiplication(x))
 
  import java.math.BigInteger
  def BigIntegerProductReducer: Reducer[BigInteger, BigIntegerMultiplication] = Reducer((x: BigInteger) => multiplication(x))

  def FirstReducer[A]: Reducer[A, FirstOption[A]] = Reducer(x => Some(x))
  def FirstOptionReducer[A]: Reducer[Option[A], FirstOption[A]] = Reducer(x => x)

  def LastReducer[A]: Reducer[A, LastOption[A]] = Reducer(x => Some(x))
  def LastOptionReducer[A]: Reducer[Option[A], LastOption[A]] = Reducer(x => x)


}
