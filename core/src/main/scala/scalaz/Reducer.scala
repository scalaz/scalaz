package scalaz

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
 */
sealed trait Reducer[C, M] {
  val monoid: Monoid[M]

  def unit(c: C): M

  def snoc(m: M, c: C): M

  def cons(c: C, m: M): M

  def semigroup: Semigroup[M] =
    monoid.semigroup

  def zero: Zero[M] =
    monoid.zero

  def z: M =
    monoid.z

  def append(a1: M, a2: => M): M =
    monoid.append(a1, a2)

  def compose[N](r: Reducer[C, N]): Reducer[C, (M, N)] = {
    implicit val m = Reducer.this.monoid
    implicit val n = r.monoid
    new Reducer[C, (M, N)] {
      val monoid = implicitly[Monoid[(M, N)]]

      override def unit(x: C) = (Reducer.this.unit(x), r.unit(x))

      override def snoc(p: (M, N), x: C) = (Reducer.this.snoc(p._1, x), r.snoc(p._2, x))

      override def cons(x: C, p: (M, N)) = (Reducer.this.cons(x, p._1), r.cons(x, p._2))
    }
  }
}

object Reducer extends Reducers {
  def apply[C, M](u: C => M, cs: C => M => M, sc: M => C => M)(implicit mm: Monoid[M]): Reducer[C, M] =
    reducer(u, cs, sc)
}

trait Reducers {

  import newtypes._
  import *._

  def reducer[C, M](u: C => M, cs: C => M => M, sc: M => C => M)(implicit mm: Monoid[M]): Reducer[C, M] =
    new Reducer[C, M] {
      val monoid = mm

      def unit(c: C) = u(c)

      def snoc(m: M, c: C): M = sc(m)(c)

      def cons(c: C, m: M): M = cs(c)(m)
    }

  /**Construct a Reducer with the given unit function and monoid **/
  def unitReducer[C, M](u: C => M)(implicit mm: Monoid[M]): Reducer[C, M] =
    new Reducer[C, M] {
      val monoid = mm

      def unit(c: C) = u(c)

      def snoc(m: M, c: C): M = mm.append(m, u(c))

      def cons(c: C, m: M): M = mm.append(u(c), m)
    }

  def unitConsReducer[C, M](u: C => M, cs: C => M => M)(implicit mm: Monoid[M]): Reducer[C, M] = new Reducer[C, M] {
    val monoid = mm

    def unit(c: C) = u(c)

    def snoc(m: M, c: C): M = mm.append(m, u(c))

    def cons(c: C, m: M): M = cs(c)(m)
  }

  def identityReducer[M](implicit mm: Monoid[M]): Reducer[M, M] = unitReducer(x => x)

  implicit def ListReducer[C]: Reducer[C, List[C]] =
    unitConsReducer(List(_), c => c :: _)

  implicit def StreamReducer[C]: Reducer[C, Stream[C]] =
    unitConsReducer(Stream(_), c => c #:: _)


  implicit def UnitReducer[C]: Reducer[C, Unit] = unitReducer((_: C) => ())

  implicit def AnyReducer: Reducer[Boolean, Boolean] = unitReducer(x => x)

  implicit def AllReducer: Reducer[Boolean, BooleanConjunction] = unitReducer(_.*-->[BooleanConjunction])

  implicit def EndoReducer[A]: Reducer[(=> A) => A, Endo[A]] = unitReducer(_.*-->[Endo[A]])

  implicit def DualReducer[A: Monoid]: Reducer[A, Dual[A]] = unitReducer(_.*-->[Dual[A]])

  implicit def IntProductReducer: Reducer[Int, IntMultiplication] = unitReducer(_.*-->[IntMultiplication])

  implicit def CharProductReducer: Reducer[Char, CharMultiplication] = unitReducer(_.*-->[CharMultiplication])

  implicit def ByteProductReducer: Reducer[Byte, ByteMultiplication] = unitReducer(_.*-->[ByteMultiplication])

  implicit def LongProductReducer: Reducer[Long, LongMultiplication] = unitReducer(_.*-->[LongMultiplication])

  implicit def ShortProductReducer: Reducer[Short, ShortMultiplication] = unitReducer(_.*-->[ShortMultiplication])

  implicit def BigIntProductReducer: Reducer[BigInt, BigIntMultiplication] = unitReducer(_.*-->[BigIntMultiplication])

  import java.math.BigInteger

  implicit def BigIntegerProductReducer: Reducer[BigInteger, BigIntegerMultiplication] = unitReducer(_.*-->[BigIntegerMultiplication])

  implicit def FirstReducer[A]: Reducer[A, FirstOption[A]] = unitReducer(x => Some(x))

  implicit def FirstOptionReducer[A]: Reducer[Option[A], FirstOption[A]] = unitReducer(x => x)

  implicit def LastReducer[A]: Reducer[A, LastOption[A]] = unitReducer(x => Some(x))

  implicit def LastOptionReducer[A]: Reducer[Option[A], LastOption[A]] = unitReducer(x => x)
}
