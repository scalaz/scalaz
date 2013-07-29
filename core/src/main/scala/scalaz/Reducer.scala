package scalaz

import scalaz.Tags.{Conjunction}


/**
 * A `Reducer[C,M]` is a [[scalaz.Monoid]]`[M]` that maps
 * values of type `C` through `unit` to values of type `M`. A `C-Reducer` may also
 * supply operations which tack on another `C` to an existing `Monoid` `M` on the left
 * or right. These specialized reductions may be more efficient in some scenarios
 * and are used when appropriate by a [[scalaz.Generator]]. The names `cons` and `snoc` work
 * by analogy to the synonymous operations in the list monoid.
 *
 * Minimal definition: `unit` or `snoc`
 *
 * Based on a Haskell library by Edward Kmett
 */
sealed trait Reducer[C, M] {
  implicit def monoid: Monoid[M]

  def unit(c: C): M

  /** Faster `append(m, unit(c))`. */
  def snoc(m: M, c: C): M

  /** Faster `append(unit(c), m)`. */
  def cons(c: C, m: M): M

  def zero: M =
    monoid.zero

  def append(a1: M, a2: => M): M =
    monoid.append(a1, a2)

  /** Distribute `C`s to `M` and `N`. */
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
sealed trait UnitReducer[C, M] extends Reducer[C, M] {
  implicit def monoid: Monoid[M]
  def unit(c: C): M

  def snoc(m: M, c: C): M = monoid.append(m, unit(c))

  def cons(c: C, m: M): M = monoid.append(unit(c), m)
}

object UnitReducer {
  /** Minimal `Reducer` derived from a monoid and `unit`. */
  def apply[C, M](u: C => M)(implicit mm: Monoid[M]): Reducer[C, M] = new UnitReducer[C, M] {
    val monoid = mm
    def unit(c: C) = u(c)
  }
}

object Reducer extends ReducerFunctions with ReducerInstances {
  /** Reducer derived from `unit`, `cons`, and `snoc`.  Permits more
    * sharing than `UnitReducer.apply`.
    */
  def apply[C, M](u: C => M, cs: C => M => M, sc: M => C => M)(implicit mm: Monoid[M]): Reducer[C, M] =
    reducer(u, cs, sc)
}

trait ReducerInstances {
  import Reducer._

  /** Collect `C`s into a list, in order. */
  implicit def ListReducer[C]: Reducer[C, List[C]] = {
    import std.list._
    unitConsReducer(List(_), c => c :: _)
  }

  /** Collect `C`s into a stream, in order. */
  implicit def StreamReducer[C]: Reducer[C, Stream[C]] = {
    import std.stream._
    unitConsReducer(Stream(_), c => c #:: _)
  }

  /** Ignore `C`s. */
  implicit def UnitReducer[C]: Reducer[C, Unit] = {
    import std.anyVal._
    unitReducer((_: C) => ())
  }

  /** Collect `C`s into a vector, in order. */
  implicit def VectorReducer[C]: Reducer[C, Vector[C]] = {
    import std.vector._
    unitReducer(c => Vector(c))
  }

  /** The "or" monoid. */
  implicit val AnyReducer: Reducer[Boolean, Boolean] = {
    implicit val B = std.anyVal.booleanInstance.disjunction
    unitReducer(x => x)
  }

  import std.anyVal._

  /** The "and" monoid. */
  implicit val AllReducer: Reducer[Boolean, Boolean @@ Conjunction] = unitReducer(b => Tag[Boolean, Conjunction](b))

  /** Accumulate endomorphisms. */
  implicit def EndoReducer[A]: Reducer[A => A, Endo[A]] = unitReducer(Endo(_))

  implicit def DualReducer[A: Monoid]: Reducer[A, A @@ Tags.Dual] = unitReducer(Tags.Dual(_: A))(Dual.dualMonoid[A])

  import Tags.{Multiplication, First, Last}

  implicit val IntProductReducer: Reducer[Int, Int @@ Multiplication] = unitReducer(i => Tag[Int, Multiplication](i))

  implicit val CharProductReducer: Reducer[Char, Char @@ Multiplication] = unitReducer(c => Tag[Char, Multiplication](c))

  implicit val ByteProductReducer: Reducer[Byte, Byte @@ Multiplication] = unitReducer(b => Tag[Byte, Multiplication](b))

  implicit val LongProductReducer: Reducer[Long, Long @@ Multiplication] = unitReducer(l => Tag[Long, Multiplication](l))

  implicit val ShortProductReducer: Reducer[Short, Short @@ Multiplication] = unitReducer(s => Tag[Short, Multiplication](s))


  implicit val BigIntProductReducer: Reducer[BigInt, BigInt @@ Multiplication] = {
    import std.math.bigInt._
    unitReducer(b => Tag[BigInt, Multiplication](b))
  }

  import std.option._

  implicit def FirstReducer[A]: Reducer[A, Option[A] @@ First] = unitReducer(a => Tag[Option[A], First](Some(a)))

  implicit def FirstOptionReducer[A]: Reducer[Option[A], Option[A] @@ First] = unitReducer(o => Tag[Option[A], First](o))

  implicit def LastReducer[A]: Reducer[A, Option[A] @@ Last] = unitReducer(a => Tag[Option[A], Last](Some(a)))

  implicit def LastOptionReducer[A]: Reducer[Option[A], Option[A] @@ Last] = unitReducer(o => Tag[Option[A], Last](o))
}

trait ReducerFunctions {

  /** Alias for [[scalaz.Reducer]]`.apply`. */
  def reducer[C, M](u: C => M, cs: C => M => M, sc: M => C => M)(implicit mm: Monoid[M]): Reducer[C, M] =
    new Reducer[C, M] {
      val monoid = mm

      def unit(c: C) = u(c)

      def snoc(m: M, c: C): M = sc(m)(c)

      def cons(c: C, m: M): M = cs(c)(m)
    }

  def foldReduce[F[_], A, B](a: F[A])(implicit f: Foldable[F], r: Reducer[A, B]): B =
    f.foldMap(a)(r.unit(_))(r.monoid)

  /** Alias for [[scalaz.UnitReducer]]`.apply`. */
  def unitReducer[C, M](u: C => M)(implicit mm: Monoid[M]): Reducer[C, M] =
    new UnitReducer[C, M] {
      val monoid = mm
      def unit(c: C) = u(c)
    }

  def unitConsReducer[C, M](u: C => M, cs: C => M => M)(implicit mm: Monoid[M]): Reducer[C, M] = new Reducer[C, M] {
    val monoid = mm

    def unit(c: C) = u(c)

    def snoc(m: M, c: C): M = mm.append(m, u(c))

    def cons(c: C, m: M): M = cs(c)(m)
  }

  /** The reducer derived from any monoid.  Not implicit because it is
    * suboptimal for most reducer applications.
    */
  def identityReducer[M](implicit mm: Monoid[M]): Reducer[M, M] = unitReducer(x => x)
}
