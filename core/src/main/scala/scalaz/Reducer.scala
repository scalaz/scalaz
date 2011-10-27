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
  implicit def monoid: Monoid[M]

  def unit(c: C): M

  def snoc(m: M, c: C): M

  def cons(c: C, m: M): M

  def zero: M =
    monoid.zero

  def append(a1: M, a2: => M): M =
    monoid.append(a1, a2)

  def compose[N](r: Reducer[C, N]): Reducer[C, (M, N)] = {
    implicit val m = Reducer.this.monoid
    implicit val n = r.monoid
    new Reducer[C, (M, N)] {

      import std.tuple._

      val monoid = Monoid[(M, N)]

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

  def reducer[C, M](u: C => M, cs: C => M => M, sc: M => C => M)(implicit mm: Monoid[M]): Reducer[C, M] =
    new Reducer[C, M] {
      val monoid = mm

      def unit(c: C) = u(c)

      def snoc(m: M, c: C): M = sc(m)(c)

      def cons(c: C, m: M): M = cs(c)(m)
    }

  def foldReduce[F[_], A, B](a: F[A])(implicit f: Traverse[F], r: Reducer[A, B]): B =
    f.foldMap(a)(r.unit(_))(r.monoid)

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

  implicit def ListReducer[C]: Reducer[C, List[C]] = {
    import std.list._
    unitConsReducer(List(_), c => c :: _)
  }

  implicit def StreamReducer[C]: Reducer[C, Stream[C]] = {
    import std.stream._
    unitConsReducer(Stream(_), c => c #:: _)
  }


  implicit def UnitReducer[C]: Reducer[C, Unit] = {
    import std.anyVal._
    unitReducer((_: C) => ())
  }

  implicit def AnyReducer: Reducer[Boolean, Boolean] = {
    implicit val B = std.anyVal.booleanInstance.disjunction
    unitReducer(x => x)
  }

  import std.anyVal._

  implicit def AllReducer: Reducer[Boolean, Boolean @@ Conjunction] = unitReducer(b => Tag[Boolean, Conjunction](b))

  // TODO
  //  implicit def EndoReducer[A]: Reducer[(=> A) => A, Endo[A]] = unitReducer(_.*-->[Endo[A]])
  //
  //  implicit def DualReducer[A: Monoid]: Reducer[A, Dual[A]] = unitReducer(_.*-->[Dual[A]])

  import Tags.{Multiplication, First, Last}

  implicit def IntProductReducer: Reducer[Int, Int @@ Multiplication] = unitReducer(i => Tag[Int, Multiplication](i))

  implicit def CharProductReducer: Reducer[Char, Char @@ Multiplication] = unitReducer(c => Tag[Char, Multiplication](c))

  implicit def ByteProductReducer: Reducer[Byte, Byte @@ Multiplication] = unitReducer(b => Tag[Byte, Multiplication](b))

  implicit def LongProductReducer: Reducer[Long, Long @@ Multiplication] = unitReducer(l => Tag[Long, Multiplication](l))

  implicit def ShortProductReducer: Reducer[Short, Short @@ Multiplication] = unitReducer(s => Tag[Short, Multiplication](s))


  implicit def BigIntProductReducer: Reducer[BigInt, BigInt @@ Multiplication] = {
    import std.math.bigInt._
    unitReducer(b => Tag[BigInt, Multiplication](b))
  }

  // TODO
  //  import java.math.BigInteger
  //
  //  implicit def BigIntegerProductReducer: Reducer[BigInteger, BigIntegerMultiplication] = unitReducer(_.*-->[BigIntegerMultiplication])

  import std.option._

  implicit def FirstReducer[A]: Reducer[A, Option[A] @@ First] = unitReducer(a => Tag[Option[A], First](Some(a)))

  implicit def FirstOptionReducer[A]: Reducer[Option[A], Option[A] @@ First] = unitReducer(o => Tag[Option[A], First](o))

  implicit def LastReducer[A]: Reducer[A, Option[A] @@ Last] = unitReducer(a => Tag[Option[A], Last](Some(a)))

  implicit def LastOptionReducer[A]: Reducer[Option[A], Option[A] @@ Last] = unitReducer(o => Tag[Option[A], Last](o))
}
