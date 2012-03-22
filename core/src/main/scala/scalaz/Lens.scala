package scalaz

import CostateT._
import collection.immutable.Stack
import collection.SeqLike

/**
 * `Lens[A, B]` provides a reference to a field of type `B` within `A`, that allows:
 *   * access: extraction of the `B`
 *   * update: creation of a new `A` with field replaced by a provided `B`
 *
 * Lenses are required to satisfy the following three laws and to be side-effect free.
 *
 * * '''identity''' `forall a b. lens.set(a,lens(a)) = a`
 * * '''retention''' `forall a b. lens(lens.set(a,b)) = b`
 * * '''double-set''' `forall a b c. lens.set(lens.set(a,b),c) = lens.set(a,c)`
 *
 * See [[http://www.youtube.com/watch?v=efv0SQNde5Q&feature=related Lenses, a Functional Imperative]]
 * See [[http://www.cs.ox.ac.uk/jeremy.gibbons/publications/colens.pdf Lenses, coalgebraically: View updates through the looking glass]]
 */
sealed trait Lens[A, B] {

  def run(a: A): Costate[B, A] // (B => A, A)

  def apply(a: A): Costate[B, A] =
    run(a)

  import StateT._
  import Lens._

  def get(a: A): B =
    run(a).pos

  def set(a: A, b: B): A =
    run(a).put(b)

  /** Modify the value viewed through the lens */
  def mod(f: B => B, a: A): A = {
    val (p, q) = run(a).run
    p(f(q))    
  }

  def =>=(f: B => B): A => A =
    mod(f, _)

  def modE(f: Endo[B]): Endo[A] =
    Endo(=>=(f.run))

  /** Modify the value viewed through the lens, a functor full of results */
  def modf[F[_]](f: B => F[B], a: A)(implicit F: Functor[F]): F[A] =
    F.map(f(get(a)))(set(a, _))

  def st: State[A, B] =
    State(s => (get(s), s))

  def :=(b: => B): State[A, B] =
    %=(_ => b)

  def %=(f: B => B): State[A, B] =
    State[A, B](a => {
      val b = f(get(a))
      (b, set(a, b))
    })

  def %==(f: B => B): State[A, Unit] =
    State[A, Unit](a => {
      ((), mod(f, a))
    })

  def %%=[C](s: State[B, C]): State[A, C] =
    State[A, C](a => {
      val (c, b) = s(get(a))
      (c, set(a, b))
    })

  def >-[C](f: B => C): State[A, C] =
    State[A, C](a => (f(get(a)), a))

  def >>-[C](f: B => State[A, C]): State[A, C] =
    State[A, C](a => f(get(a)).apply(a))

  def ->>-[C](f: => State[A, C]): State[A, C] =
    >>-(_ => f)

  /** Lenses can be composed */
  def compose[C](that: Lens[C, A]): Lens[C, B] =
    lens[C, B](c => {
      val (ac, a) = that.run(c).run
      val (ba, b) = run(a).run
      costate[B, C](ac compose ba, b)
    })

  /** alias for `compose` */
  def >=>[C](that: Lens[C, A]): Lens[C, B] = compose(that)

  def andThen[C](that: Lens[B, C]): Lens[A, C] =
    that compose this

  /** alias for `andThen` */
  def <=<[C](that: Lens[B, C]): Lens[A, C] = andThen(that)

  /** Two lenses that view a value of the same type can be joined */
  def sum[C](that: => Lens[C, B]): Lens[Either[A, C], B] =
    lensGG[Either[A, C], B](
    {
      case Left(a)  => get(a)
      case Right(b) => that.get(b)
    }, {
      case (Left(a), b)  => Left(set(a, b))
      case (Right(c), b) => Right(that.set(c, b))
    })

  /** Alias for `sum` */
  def |||[C](that: => Lens[C, B]): Lens[Either[A, C], B]= sum(that)

  /** Two disjoint lenses can be paired */
  def product[C, D](that: Lens[C, D]): Lens[(A, C), (B, D)] =
    lensGG[(A, C), (B, D)](
      ac => (get(ac._1), that.get(ac._2)),
      (ac, bd) => (set(ac._1, bd._1), that.set(ac._2, bd._2))
    )

  /** alias for `product` */
  def ***[C, D](that: Lens[C, D]): Lens[(A, C), (B, D)] = product(that)

  /** A homomorphism of lens categories */
  def partial: PLens[A, B] =
    PLens.plens(a => Some(run(a)))

  /** alias for `partial` */
  def unary_~ : PLens[A, B] =
    partial

  trait LensLaw {
    def identity(a: A)(implicit A: Equal[A]): Boolean = A.equal(set(a, get(a)), a)
    def retention(a: A, b: B)(implicit B: Equal[B]): Boolean = B.equal(get(set(a, b)), b)
    def doubleSet(a: A, b1: B, b2: B)(implicit A: Equal[A]) = A.equal(set(set(a, b1), b2), set(a, b2))
  }

  def lensLaw = new LensLaw {}
}

object Lens extends LensFunctions with LensInstances {
  def apply[A, B](r: A => Costate[B, A]): Lens[A, B] =
    lens(r)
}

trait LensInstances {

  import State._
  import Lens._

  implicit def lensCategory: Category[Lens] with Choice[Lens] with Split[Lens] with Codiagonal[Lens] = new Category[Lens] with Choice[Lens] with Split[Lens] with Codiagonal[Lens] {
    def compose[A, B, C](f: Lens[B, C], g: Lens[A, B]): Lens[A, C] = f compose g
    def id[A]: Lens[A, A] = Lens.lensId[A]
    def choice[A, B, C](f: => Lens[A, C], g: => Lens[B, C]): Lens[Either[A,  B], C] =
      Lens {
        case Left(a) => {
          val x = f run a
          costate(w => Left(x put w), x.pos)
        }
        case Right(b) => {
          val y = g run b
          costate(w => Right(y put w), y.pos)
        }
      }
    def split[A, B, C, D](f: Lens[A, B], g: Lens[C, D]): Lens[(A,  C), (B, D)] =
      f *** g
    def codiagonal[A]: Lens[Either[A,  A], A] =
      codiagLens
  }

  /** Lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def LensState[A, B](lens: Lens[A, B]): State[A, B] =
    lens.st

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple2Lens[S, A, B](lens: Lens[S, (A, B)]) = (
    lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a), s)),
    lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a), s))
    )

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple3Lens[S, A, B, C](lens: Lens[S, (A, B, C)]) = (
    lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a), s)),
    lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a), s)),
    lensG[S, C](s => lens.get(s)._3, s => a => lens.mod(t => t copy (_3 = a), s))
    )

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple4Lens[S, A, B, C, D](lens: Lens[S, (A, B, C, D)]) = (
    lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a), s)),
    lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a), s)),
    lensG[S, C](s => lens.get(s)._3, s => a => lens.mod(t => t copy (_3 = a), s)),
    lensG[S, D](s => lens.get(s)._4, s => a => lens.mod(t => t copy (_4 = a), s))
    )

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple5Lens[S, A, B, C, D, E](lens: Lens[S, (A, B, C, D, E)]) = (
    lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a), s)),
    lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a), s)),
    lensG[S, C](s => lens.get(s)._3, s => a => lens.mod(t => t copy (_3 = a), s)),
    lensG[S, D](s => lens.get(s)._4, s => a => lens.mod(t => t copy (_4 = a), s)),
    lensG[S, E](s => lens.get(s)._5, s => a => lens.mod(t => t copy (_5 = a), s))
    )

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple6Lens[S, A, B, C, D, E, F](lens: Lens[S, (A, B, C, D, E, F)]) = (
    lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a), s)),
    lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a), s)),
    lensG[S, C](s => lens.get(s)._3, s => a => lens.mod(t => t copy (_3 = a), s)),
    lensG[S, D](s => lens.get(s)._4, s => a => lens.mod(t => t copy (_4 = a), s)),
    lensG[S, E](s => lens.get(s)._5, s => a => lens.mod(t => t copy (_5 = a), s)),
    lensG[S, F](s => lens.get(s)._6, s => a => lens.mod(t => t copy (_6 = a), s))
    )

  /** Enriches lenses that view tuples with field accessors */
  implicit def tuple7Lens[S, A, B, C, D, E, F, G](lens: Lens[S, (A, B, C, D, E, F, G)]) = (
    lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a), s)),
    lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a), s)),
    lensG[S, C](s => lens.get(s)._3, s => a => lens.mod(t => t copy (_3 = a), s)),
    lensG[S, D](s => lens.get(s)._4, s => a => lens.mod(t => t copy (_4 = a), s)),
    lensG[S, E](s => lens.get(s)._5, s => a => lens.mod(t => t copy (_5 = a), s)),
    lensG[S, F](s => lens.get(s)._6, s => a => lens.mod(t => t copy (_6 = a), s)),
    lensG[S, G](s => lens.get(s)._7, s => a => lens.mod(t => t copy (_7 = a), s))
    )

  /** A lens that views a Set can provide the appearance of in place mutation */
  implicit def setLens[S, K](lens: Lens[S, Set[K]]) =
    SetLens[S, K](lens)

  case class SetLens[S, K](lens: Lens[S, Set[K]]) {
    /** Setting the value of this lens will change whether or not it is present in the set */
    def contains(key: K) = lensG[S, Boolean](
      s => lens.get(s).contains(key),
      s => b => lens.mod(m => if (b) m + key else m - key, s)
    )

    def &=(that: Set[K]): State[S, Set[K]] =
      lens %= (_ & that)

    def &~=(that: Set[K]): State[S, Set[K]] =
      lens %= (_ &~ that)

    def |=(that: Set[K]): State[S, Set[K]] =
      lens %= (_ | that)

    def +=(elem: K): State[S, Set[K]] =
      lens %= (_ + elem)

    def +=(elem1: K, elem2: K, elems: K*): State[S, Set[K]] =
      lens %= (_ + elem1 + elem2 ++ elems)

    def ++=(xs: TraversableOnce[K]): State[S, Set[K]] =
      lens %= (_ ++ xs)

    def -=(elem: K): State[S, Set[K]] =
      lens %= (_ - elem)

    def -=(elem1: K, elem2: K, elems: K*): State[S, Set[K]] =
      lens %= (_ - elem1 - elem2 -- elems)

    def --=(xs: TraversableOnce[K]): State[S, Set[K]] =
      lens %= (_ -- xs)
  }

  /** A lens that views an immutable Map type can provide a mutable.Map-like API via State */
  case class MapLens[S, K, V](lens: Lens[S, Map[K, V]]) {
    /** Allows both viewing and setting the value of a member of the map */
    def member(k: K): Lens[S, Option[V]] = lensG[S, Option[V]](
      s => lens.get(s).get(k),
      s => opt => lens.mod(m => opt match {
        case Some(v) => m + (k -> v)
        case None    => m - k
      }, s))

    /** This lens has undefined behavior when accessing an element not present in the map! */
    def at(k: K): Lens[S, V] =
      lensG[S, V](lens.get(_)(k), s => v => lens.mod(_ + (k -> v), s))

    def +=(elem1: (K, V), elem2: (K, V), elems: (K, V)*): State[S, Map[K, V]] =
      lens %= (_ + elem1 + elem2 ++ elems)

    def +=(elem: (K, V)): State[S, Map[K, V]] =
      lens %= (_ + elem)

    def ++=(xs: TraversableOnce[(K, V)]): State[S, Map[K, V]] =
      lens %= (_ ++ xs)

    def update(key: K, value: V): State[S, Unit] =
      lens %== (_.updated(key, value))

    def -=(elem: K): State[S, Map[K, V]] =
      lens %= (_ - elem)

    def -=(elem1: K, elem2: K, elems: K*): State[S, Map[K, V]] =
      lens %= (_ - elem1 - elem2 -- elems)

    def --=(xs: TraversableOnce[K]): State[S, Map[K, V]] =
      lens %= (_ -- xs)
  }

  implicit def mapLens[S, K, V](lens: Lens[S, Map[K, V]]) = MapLens[S, K, V](lens)

  /** Provide the appearance of a mutable-like API for sorting sequences through a lens */
  case class SeqLikeLens[S, A, Repr <: SeqLike[A, Repr]](lens: Lens[S, Repr]) {
    def sortWith(lt: (A, A) => Boolean): State[S, Unit] =
      lens %== (_ sortWith lt)

    def sortBy[B: math.Ordering](f: A => B): State[S, Unit] =
      lens %== (_ sortBy f)

    def sort[B >: A](implicit ord: math.Ordering[B]) =
      lens %== (_.sorted[B]): State[S, Unit]
  }

  implicit def seqLikeLens[S, A, Repr <: SeqLike[A, Repr]](lens: Lens[S, Repr]) =
    SeqLikeLens[S, A, Repr](lens)

  implicit def seqLens[S, A](lens: Lens[S, scala.collection.immutable.Seq[A]]) =
    seqLikeLens[S, A, scala.collection.immutable.Seq[A]](lens)

  /** Provide an imperative-seeming API for stacks viewed through a lens */
  case class StackLens[S, A](lens: Lens[S, Stack[A]]) {
    def push(elem1: A, elem2: A, elems: A*): State[S, Unit] =
      lens %== (_ push elem1 push elem2 pushAll elems)

    def push1(elem: A): State[S, Unit] =
      lens %== (_ push elem)

    def pop: State[S, Unit] =
      lens %== (_ pop)

    def pop2: State[S, A] =
      lens %%= (State(_.pop2))

    def top: State[S, A] =
      lens >- (_.top)

    def length: State[S, Int] =
      lens >- (_.length)
  }

  implicit def stackLens[S, A](lens: Lens[S, Stack[A]]) =
    StackLens[S, A](lens)


  import collection.immutable.Queue

  /** Provide an imperative-seeming API for queues viewed through a lens */
  case class QueueLens[S, A](lens: Lens[S, Queue[A]]) {
    def enqueue(elem: A): State[S, Unit] =
      lens %== (_ enqueue elem)

    def dequeue: State[S, A] =
      lens %%= (State(_.dequeue))

    def length: State[S, Int] =
      lens >- (_.length)
  }

  implicit def queueLens[S, A](lens: Lens[S, Queue[A]]) =
    QueueLens[S, A](lens)

  /** Provide an imperative-seeming API for arrays viewed through a lens */
  case class ArrayLens[S, A](lens: Lens[S, Array[A]]) {
    def at(n: Int): (Lens[S, A]) =
      lensG[S, A](
        s => lens.get(s)(n),
        s => v => lens.mod(array => {
          val copy = array.clone()
          copy.update(n, v)
          copy
        }, s)
      )

    def length: State[S, Int] =
      lens >- (_.length)
  }

  implicit def arrayLens[S, A](lens: Lens[S, Array[A]]) =
    ArrayLens[S, A](lens)

  /** Allow the illusion of imperative updates to numbers viewed through a lens */
  case class NumericLens[S, N: Numeric](lens: Lens[S, N], num: Numeric[N]) {
    def +=(that: N): State[S, N] =
      lens %= (num.plus(_, that))

    def -=(that: N): State[S, N] =
      lens %= (num.minus(_, that))

    def *=(that: N): State[S, N] =
      lens %= (num.times(_, that))
  }

  implicit def numericLens[S, N: Numeric](lens: Lens[S, N]) =
    NumericLens[S, N](lens, implicitly[Numeric[N]])

  /** Allow the illusion of imperative updates to numbers viewed through a lens */
  case class FractionalLens[S, F](lens: Lens[S, F], frac: Fractional[F]) {
    def /=(that: F): State[S, F] =
      lens %= (frac.div(_, that))
  }

  implicit def fractionalLens[S, F: Fractional](lens: Lens[S, F]) =
    FractionalLens[S, F](lens, implicitly[Fractional[F]])

  /** Allow the illusion of imperative updates to numbers viewed through a lens */
  case class IntegralLens[S, I](lens: Lens[S, I], ig: Integral[I]) {
    def %=(that: I): State[S, I] =
      lens %= (ig.quot(_, that))
  }

  implicit def integralLens[S, I: Integral](lens: Lens[S, I]) =
    IntegralLens[S, I](lens, implicitly[Integral[I]])
}

trait LensFunctions {

  import CostateT._

  /** The sunglasses operator, an alias for `Lens` */
  type @-@[A, B] =
  Lens[A, B]

  def lens[A, B](r: A => Costate[B, A]): Lens[A, B] = new Lens[A, B] {
    def run(a: A): Costate[B, A] = r(a)
  }

  def lensG[A, B](get: A => B, set: A => B => A): Lens[A, B] =
    lens(a => costate((set(a), get(a))))

  def lensGG[A, B](get: A => B, set: (A, B) => A): Lens[A, B] =
    lensG(get, a => b => set(a, b))

  /** The identity lens for a given object */
  def lensId[A]: Lens[A, A] =
    lensG(z => z, _ => z => z)

  /** The trivial lens that can retrieve Unit from anything */
  def trivialLens[A]: Lens[A, Unit] =
    lensG[A, Unit](_ => (), a => _ => a)

  /** A lens that discards the choice of Right or Left from Either */
  def codiagLens[A]: Lens[Either[A, A], A] =
    lensId[A] ||| lensId[A]

  /** Access the first field of a tuple */
  def firstLens[A, B]: Lens[(A, B), A] =
    lensG[(A, B), A](_._1, ab => a => (a, ab._2))

  /** Access the second field of a tuple */
  def secondLens[A, B]: Lens[(A, B), B] =
    lensG[(A, B), B](_._2, ab => b => (ab._1, b))

  /** Access the first field of a lazy tuple */
  def lazyFirstLens[A, B]: Lens[LazyTuple2[A, B], A] =
    lensG[LazyTuple2[A, B], A](_._1, ab => a => LazyTuple2(a, ab._2))

  /** Access the second field of a lazy tuple */
  def lazySecondLens[A, B]: Lens[LazyTuple2[A, B], B] =
    lensG[LazyTuple2[A, B], B](_._2, ab => b => LazyTuple2(ab._1, b))

  def nelHeadLens[A]: NonEmptyList[A] @-@ A =
    lens(l => costate(NonEmptyList.nel(_, l.tail), l.head))

  def nelTailLens[A]: NonEmptyList[A] @-@ List[A] =
    lens(l => costate(NonEmptyList.nel(l.head, _), l.tail))

}
