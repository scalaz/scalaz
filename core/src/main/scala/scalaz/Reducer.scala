package scalaz

import scala.annotation.tailrec
import scalaz.Free.{Trampoline, return_, suspend}
import scalaz.Maybe.Just
import scalaz.Tags.Conjunction


/**
 * A `Reducer[C,M]` is a [[scalaz.Semigroup]]`[M]` that maps
 * values of type `C` through `unit` to values of type `M`. A `C-Reducer` may also
 * supply operations which tack on another `C` to an existing `Semigroup` `M` on the left
 * or right. These specialized reductions may be more efficient in some scenarios
 * and are used when appropriate by a [[scalaz.Generator]]. The names `cons` and `snoc` work
 * by analogy to the synonymous operations in the list semigroup.
 *
 * Minimal definition: `unit` or `snoc`
 *
 * Based on the Reducer Haskell library by Edward Kmett
 * (https://hackage.haskell.org/package/reducers).
 */
trait Reducer[C, M] {
  implicit def semigroup: Semigroup[M]

  def unit(c: C): M

  /** Faster `append(m, unit(c))`. */
  def snoc(m: M, c: C): M

  /** Faster `append(unit(c), m)`. */
  def cons(c: C, m: M): M

  def append(a1: M, a2: => M): M =
    semigroup.append(a1, a2)

  /** Distribute `C`s to `M` and `N`. */
  def compose[N](r: Reducer[C, N]): Reducer[C, (M, N)] = {
    implicit val m = Reducer.this.semigroup
    implicit val n = r.semigroup
    new Reducer[C, (M, N)] {

      import std.tuple._

      val semigroup = Semigroup[(M, N)]

      override def unit(x: C) = (Reducer.this.unit(x), r.unit(x))

      override def snoc(p: (M, N), x: C) = (Reducer.this.snoc(p._1, x), r.snoc(p._2, x))

      override def cons(x: C, p: (M, N)) = (Reducer.this.cons(x, p._1), r.cons(x, p._2))
    }
  }

  def unfoldlOpt[B](seed: B)(f: B => Maybe[(B, C)]): Maybe[M] =
    defaultUnfoldlOpt(seed)(f)

  @inline private def defaultUnfoldlOpt[B](seed: B)(f: B => Maybe[(B, C)]): Maybe[M] = {
    @tailrec
    def rec(seed: B, acc: M): M = f(seed) match {
      case Just((b, c)) => rec(b, cons(c, acc))
      case _ => acc
    }
    f(seed) map { case (b, c) => rec(b, unit(c)) }
  }

  def unfoldl[B](seed: B)(f: B => Maybe[(B, C)])(implicit M: Monoid[M]): M =
    unfoldlOpt(seed)(f) getOrElse M.zero

  def unfoldrOpt[B](seed: B)(f: B => Maybe[(C, B)]): Maybe[M] =
    defaultUnfoldrOpt(seed)(f)

  @inline private def defaultUnfoldrOpt[B](seed: B)(f: B => Maybe[(C, B)]): Maybe[M] = {
    @tailrec
    def rec(acc: M, seed: B): M = f(seed) match {
      case Just((c, b)) => rec(snoc(acc, c), b)
      case _ => acc
    }
    f(seed) map { case (c, b) => rec(unit(c), b) }
  }

  def unfoldr[B](seed: B)(f: B => Maybe[(C, B)])(implicit M: Monoid[M]): M =
    unfoldrOpt(seed)(f) getOrElse M.zero

  trait ReducerLaw {
    def consCorrectness(c: C, m: M)(implicit E: Equal[M]): Boolean =
      E.equal(cons(c, m), append(unit(c), m))

    def snocCorrectness(m: M, c: C)(implicit E: Equal[M]): Boolean =
      E.equal(snoc(m, c), append(m, unit(c)))

    def unfoldlOptConsistency[B](seed: B, f: B => Maybe[(B, C)])(implicit E: Equal[M]): Boolean = {
      val g: ((Int, B)) => Maybe[((Int, B), C)] = { case (i, b) =>
        if(i > 0) f(b) map { case (b, c) => ((i-1, b), c) }
        else Maybe.empty
      }
      val limit = 4 // to prevent infinite unfolds
      Equal[Maybe[M]].equal(unfoldlOpt((limit, seed))(g), defaultUnfoldlOpt((limit, seed))(g))
    }

    def unfoldrOptConsistency[B](seed: B, f: B => Maybe[(C, B)])(implicit E: Equal[M]): Boolean = {
      val g: ((Int, B)) => Maybe[(C, (Int, B))] = { case (i, b) =>
        if(i > 0) f(b) map { case (c, b) => (c, (i-1, b)) }
        else Maybe.empty
      }
      val limit = 4 // to prevent infinite unfolds
      Equal[Maybe[M]].equal(unfoldrOpt((limit, seed))(g), defaultUnfoldrOpt((limit, seed))(g))
    }
  }
  def reducerLaw = new ReducerLaw {}
}
sealed abstract class UnitReducer[C, M] extends Reducer[C, M] {
  implicit def semigroup: Semigroup[M]
  def unit(c: C): M

  def snoc(m: M, c: C): M = semigroup.append(m, unit(c))

  def cons(c: C, m: M): M = semigroup.append(unit(c), m)

  override def unfoldlOpt[B](seed: B)(f: B => Maybe[(B, C)]): Maybe[M] =
    semigroup.unfoldlSumOpt(seed)(f(_) map { case (b, c) => (b, unit(c)) })

  override def unfoldrOpt[B](seed: B)(f: B => Maybe[(C, B)]): Maybe[M] =
    semigroup.unfoldrSumOpt(seed)(f(_) map { case (c, b) => (unit(c), b) })
}

object UnitReducer {
  /** Minimal `Reducer` derived from a semigroup and `unit`. */
  def apply[C, M](u: C => M)(implicit M: Semigroup[M]): Reducer[C, M] = new UnitReducer[C, M] {
    val semigroup = M
    def unit(c: C) = u(c)
  }
}

object Reducer extends ReducerInstances {
  /** Reducer derived from `unit`, `cons`, and `snoc`.  Permits more
    * sharing than `UnitReducer.apply`.
    */
  def apply[C, M](u: C => M, cs: (C, M) => M, sc: (M, C) => M)(implicit M: Semigroup[M]): Reducer[C, M] =
    reducer(u, cs, sc)
}

sealed abstract class ReducerInstances {

  /** Collect `C`s into a list, in order. */
  implicit def ListReducer[C]: Reducer[C, List[C]] = {
    import std.list._
    unitConsReducer(_ :: Nil, _ :: _)
  }

  def ReverseListReducer[C]: Reducer[C, List[C]] = {
    import std.list._
    reducer(_ :: Nil, (c, cs) => cs :+ c, (cs, c) => c :: cs)
  }

  /** Collect `C`s into an ilist, in order. */
  implicit def IListReducer[C]: Reducer[C, IList[C]] = {
    unitConsReducer(_ :: INil(), _ :: _)
  }

  def ReverseIListReducer[C]: Reducer[C, IList[C]] = {
    reducer(_ :: INil(), (c, cs) => cs :+ c, (cs, c) => c :: cs)
  }

  /** Collect `C`s into an NonEmptyList, in order. */
  implicit def NonEmptyListReducer[C]: Reducer[C, NonEmptyList[C]] = {
    unitConsReducer(NonEmptyList.nel(_, INil()),  _ <:: _)
  }
  
  def ReverseNonEmptyListReducer[C]: Reducer[C, NonEmptyList[C]] = {
    reducer(NonEmptyList.nel(_, INil()), (c, cs) => NonEmptyList.nel(cs.head, cs.tail :+ c), (cs, c) => c <:: cs)
  }

  /** Collect `C`s into a stream, in order. */
  implicit def StreamReducer[C]: Reducer[C, Stream[C]] = {
    import std.stream._
    import Stream._
    unitLazyConsReducer(cons(_, empty): Stream[C], cons(_, _))
  }

  def ReverseStreamReducer[C]: Reducer[C, Stream[C]] = {
    import std.stream._
    import Stream._
    reducer(cons(_, empty), (c, cs) => cs :+ c, (cs, c) => cons(c, cs))    
  }

  def ReverseEphemeralStreamReducer[C]: Reducer[C, EphemeralStream[C]] = {
    import EphemeralStream._
    reducer(cons(_, emptyEphemeralStream), (c, cs) => cs ++ cons(c, emptyEphemeralStream), (cs, c) => cons(c, cs))    
  }

  /** Ignore `C`s. */
  implicit def UnitReducer[C]: Reducer[C, Unit] = {
    import std.anyVal._
    unitReducer((_: C) => ())
  }

  /** Collect `C`s into a vector, in order. */
  implicit def VectorReducer[C]: Reducer[C, Vector[C]] = new Reducer[C, Vector[C]]{
    val semigroup: Semigroup[Vector[C]] = std.vector.vectorMonoid[C]
    def cons(c: C, m: Vector[C]) = c +: m
    def snoc(m: Vector[C], c: C) = m :+ c
    def unit(c: C) = Vector(c)
  }

  /** The "or" semigroup. */
  implicit val AnyReducer: Reducer[Boolean, Boolean] = {
    implicit val B = std.anyVal.booleanInstance.disjunction
    unitReducer(x => x)
  }

  import std.anyVal._

  /** The "and" semigroup. */
  implicit val AllReducer: Reducer[Boolean, Boolean @@ Conjunction] = unitReducer(b => Tag[Boolean, Conjunction](b))

  /** Accumulate endomorphisms. */
  implicit def EndoReducer[A]: Reducer[A => A, Endo[A]] = unitReducer(Endo(_))

  implicit def DualReducer[A: Semigroup]: Reducer[A, A @@ Tags.Dual] = unitReducer(Tags.Dual(_: A))(Dual.dualSemigroup[A])

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

  /** Alias for [[scalaz.Reducer]]`.apply`. */
  def reducer[C, M](u: C => M, cs: (C, M) => M, sc: (M, C) => M)(implicit sm: Semigroup[M]): Reducer[C, M] =
    new Reducer[C, M] {
      val semigroup = sm

      def unit(c: C) = u(c)

      def snoc(m: M, c: C): M = sc(m, c)

      def cons(c: C, m: M): M = cs(c, m)
    }

  def foldReduce[F[_], A, B: Monoid](a: F[A])(implicit f: Foldable[F], r: Reducer[A, B]): B =
    f.foldMap(a)(r.unit)

  /** Alias for [[scalaz.UnitReducer]]`.apply`. */
  def unitReducer[C, M](u: C => M)(implicit mm: Semigroup[M]): Reducer[C, M] =
    new UnitReducer[C, M] {
      val semigroup = mm
      def unit(c: C) = u(c)
    }

  def unitConsReducer[C, M](u: C => M, cs: (C, M) => M)(implicit sm: Semigroup[M]): Reducer[C, M] = new Reducer[C, M] {
    val semigroup = sm

    def unit(c: C) = u(c)

    def snoc(m: M, c: C): M = sm.append(m, u(c))

    def cons(c: C, m: M): M = cs(c, m)

    override def unfoldr[B](seed: B)(f: B => Maybe[(C, B)])(implicit M: Monoid[M]): M = {
      import  std.function._
      def go(s: B, f: B => Maybe[(C, B)]): Trampoline[M] = f(s) match {
        case Just((c, b)) => suspend(go(b, f)) map (m => cs(c, m))
        case _ => return_[Function0, M](M.zero)
      }
      go(seed, f).run
    }
  }

  def unitLazyConsReducer[C, M](u: C => M, cs: (C, => M) => M)(implicit sm: Semigroup[M]): Reducer[C, M] = new Reducer[C, M] {
    val semigroup = sm

    def unit(c: C) = u(c)

    def snoc(m: M, c: C): M = sm.append(m, u(c))

    def cons(c: C, m: M): M = cs(c, m)

    override def unfoldr[B](seed: B)(f: B => Maybe[(C, B)])(implicit M: Monoid[M]): M = {
      def unfold(s: B): M = f(s) match {
        case Just((a, r)) => cs(a, unfold(r))
        case _ => M.zero
      }
      unfold(seed)
    }
  }

  /** The reducer derived from any semigroup.  Not implicit because it is
    * suboptimal for most reducer applications.
    */
  def identityReducer[M](implicit mm: Semigroup[M]): Reducer[M, M] =
    new Reducer[M, M] {
      def semigroup = mm
      def unit(c: M): M = c
      def cons(c: M, m: M): M = append(c, m)
      def snoc(m: M, c: M): M = append(m, c)

      override def unfoldlOpt[B](seed: B)(f: B => Maybe[(B, M)]): Maybe[M] =
        mm.unfoldlSumOpt(seed)(f)

      override def unfoldrOpt[B](seed: B)(f: B => Maybe[(M, B)]): Maybe[M] =
        mm.unfoldrSumOpt(seed)(f)
    }
}
