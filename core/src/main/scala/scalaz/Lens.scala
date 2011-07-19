package scalaz

import CoStateT._
import collection.immutable.Stack
import collection.SeqLike

/**
 * Lenses are required to satisfy the following two laws and to be side-effect free.
 *
 * <p>
 * All instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a b. lens.set(a,lens(a)) = a</code></li>
 * <li><strong>retention</strong><br/><code>forall a b. lens(lens.set(a,b)) = b</code></li>
 * <li><strong>double-set</strong><br/><code>forall a b c. lens.set(lens.set(a,b),c) = lens.set(a,c)</code></li>
 * </ol>
 * </p>
 */
sealed trait Lens[A, B] {
  val run: A => CoState[B, A]

  import StateT._
  import Lens._

  def *->* : (({type λ[α] = A @@ α})#λ *->* B) =
    scalaz.*->*.!**->**![({type λ[α] = A @@ α})#λ, B](this)

  def get: A => B =
    a =>
      run(a).pos

  def apply(a: A): B =
    get(a)

  def set: A => B => A =
    a =>
      run(a).put

  /**Modify the value viewed through the lens */
  def mod(f: B => B): A => A =
    a =>
      set(a)(f(get(a)))

  /**Modify the value viewed through the lens, a functor full of results */
  def modf[F[_]](f: B => F[B])(implicit ftr: Functor[F]): A => F[A] =
    a =>
      ftr.fmap((b: B) => set(a)(b))(f(get(a)))

  def st: State[A, B] =
    state(s => (get(s), s))

  def :=(b: => B): State[A, B] =
    %=(_ => b)

  def %=(f: B => B): State[A, B] =
    state[A, B](a => {
      val b = f(get(a))
      (b, set(a)(b))
    })

  def %==(f: B => B): State[A, Unit] =
    state[A, Unit](a => {
      ((), mod(f)(a))
    })

  def %%=[C](s: State[B, C]): State[A, C] =
    state[A, C](a => {
      val (c, b) = s.run(get(a))
      (c, set(a)(b))
    })

  def >-[C](f: B => C): State[A, C] =
    state[A, C](a => (f(get(a)), a))

  def >>-[C](f: B => State[A, C]): State[A, C] =
    state[A, C](a => f(get(a)).run(a))

  def ->>-[C](f: => State[A, C]): State[A, C] =
    >>-(_ => f)

  /**Lenses can be composed */
  def >=>[C](that: C @@ A): (C @@ B) =
    lens[C, B](c => {
      val (f, a) = that.run(c).run
      val (g, b) = run(a).run
      coState[B, C](f compose g, b)
    })

  /**Lenses can be composed */
  def <=<[C](that: B @@ C): (A @@ C) =
    that >=> this

  /**Two lenses that view a value of the same type can be joined */
  def |||[C](that: C @@ B): (Either[A, C] @@ B) =
    lensGG[Either[A, C], B](
    {
      case Left(a) => get(a)
      case Right(b) => that.get(b)
    }, {
      case (Left(a), b) => Left(set(a)(b))
      case (Right(c), b) => Right(that.set(c)(b))
    }
    )

  /**Two disjoint lenses can be paired */
  def ***[C, D](that: C @@ D): ((A, C) @@ (B, D)) =
    lensGG[(A, C), (B, D)](
      ac => (get(ac._1), that.get(ac._2)),
      (ac, bd) => (set(ac._1)(bd._1), that.set(ac._2)(bd._2))
    )
}

object Lens extends Lenss {
  def apply[A, B](r: A => CoState[B, A]): (A @@ B) =
    lens(r)
}

trait Lenss {

  import StateT._

  type @@[A, B] =
  Lens[A, B]

  def lens[A, B](r: A => CoState[B, A]): (A @@ B) = new (A @@ B) {
    val run = r
  }

  def lensG[A, B](get: A => B, set: A => B => A): (A @@ B) =
    lens(a => coState((set(a), get(a))))

  def lensGG[A, B](get: A => B, set: (A, B) => A): (A @@ B) =
    lensG(get, a => b => set(a, b))

  /**The identity lens for a given object */
  def lensId[A]: (A @@ A) =
    lensG(z => z, _ => z => z)

  /**The trivial lens that can retrieve Unit from anything */
  def trivialLens[A]: (A @@ Unit) =
    lensG[A, Unit](_ => (), a => _ => a)

  /**A lens that discards the choice of Right or Left from Either */
  def codiagLens[A]: (Either[A, A] @@ A) =
    lensId[A] ||| lensId[A]

  /**Access the first field of a tuple */
  def firstLens[A, B]: ((A, B) @@ A) =
    lensG[(A, B), A](_._1, ab => a => (a, ab._2))

  /**Access the second field of a tuple */
  def secondLens[A, B]: ((A, B) @@ B) =
    lensG[(A, B), B](_._2, ab => b => (ab._1, b))

  /**Lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def LensState[A, B](lens: Lens[A, B]): State[A, B] =
    lens.st

  /**Enriches lenses that view tuples with field accessors */
  implicit def tuple2Lens[S, A, B](lens: Lens[S, (A, B)]) = (
      lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a))(s)),
      lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a))(s))
      )

  /**Enriches lenses that view tuples with field accessors */
  implicit def tuple3Lens[S, A, B, C](lens: Lens[S, (A, B, C)]) = (
      lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a))(s)),
      lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a))(s)),
      lensG[S, C](s => lens.get(s)._3, s => a => lens.mod(t => t copy (_3 = a))(s))
      )

  /**Enriches lenses that view tuples with field accessors */
  implicit def tuple4Lens[S, A, B, C, D](lens: Lens[S, (A, B, C, D)]) = (
      lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a))(s)),
      lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a))(s)),
      lensG[S, C](s => lens.get(s)._3, s => a => lens.mod(t => t copy (_3 = a))(s)),
      lensG[S, D](s => lens.get(s)._4, s => a => lens.mod(t => t copy (_4 = a))(s))
      )

  /**Enriches lenses that view tuples with field accessors */
  implicit def tuple5Lens[S, A, B, C, D, E](lens: Lens[S, (A, B, C, D, E)]) = (
      lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a))(s)),
      lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a))(s)),
      lensG[S, C](s => lens.get(s)._3, s => a => lens.mod(t => t copy (_3 = a))(s)),
      lensG[S, D](s => lens.get(s)._4, s => a => lens.mod(t => t copy (_4 = a))(s)),
      lensG[S, E](s => lens.get(s)._5, s => a => lens.mod(t => t copy (_5 = a))(s))
      )

  /**Enriches lenses that view tuples with field accessors */
  implicit def tuple6Lens[S, A, B, C, D, E, F](lens: Lens[S, (A, B, C, D, E, F)]) = (
      lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a))(s)),
      lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a))(s)),
      lensG[S, C](s => lens.get(s)._3, s => a => lens.mod(t => t copy (_3 = a))(s)),
      lensG[S, D](s => lens.get(s)._4, s => a => lens.mod(t => t copy (_4 = a))(s)),
      lensG[S, E](s => lens.get(s)._5, s => a => lens.mod(t => t copy (_5 = a))(s)),
      lensG[S, F](s => lens.get(s)._6, s => a => lens.mod(t => t copy (_6 = a))(s))
      )

  /**Enriches lenses that view tuples with field accessors */
  implicit def tuple7Lens[S, A, B, C, D, E, F, G](lens: Lens[S, (A, B, C, D, E, F, G)]) = (
      lensG[S, A](s => lens.get(s)._1, s => a => lens.mod(t => t copy (_1 = a))(s)),
      lensG[S, B](s => lens.get(s)._2, s => a => lens.mod(t => t copy (_2 = a))(s)),
      lensG[S, C](s => lens.get(s)._3, s => a => lens.mod(t => t copy (_3 = a))(s)),
      lensG[S, D](s => lens.get(s)._4, s => a => lens.mod(t => t copy (_4 = a))(s)),
      lensG[S, E](s => lens.get(s)._5, s => a => lens.mod(t => t copy (_5 = a))(s)),
      lensG[S, F](s => lens.get(s)._6, s => a => lens.mod(t => t copy (_6 = a))(s)),
      lensG[S, G](s => lens.get(s)._7, s => a => lens.mod(t => t copy (_7 = a))(s))
      )

  /**A lens that views a Set can provide the appearance of in place mutation */
  implicit def setLens[S, K](lens: S @@ Set[K]) =
    SetLens[S, K](lens)

  case class SetLens[S, K](lens: S @@ Set[K]) {
    /**Setting the value of this lens will change whether or not it is present in the set */
    def contains(key: K) = lensG[S, Boolean](
      s => lens.get(s).contains(key),
      s => b => lens.mod(m => if (b) m + key else m - key)(s)
    )

    def &=(that: Set[K]): State[S, Set[K]] =
      lens %= (_ & that)

    def &~=(that: Set[K]): State[S, Set[K]] =
      lens %= (_ &~ that)

    def |=(that: Set[K]): State[S, Set[K]] =
      lens %= (_ | that)

    def +=(elem: K) =
      lens %= (_ + elem)

    def +=(elem1: K, elem2: K, elems: K*) =
      lens %= (_ + elem1 + elem2 ++ elems)

    def ++=(xs: TraversableOnce[K]) =
      lens %= (_ ++ xs)

    def -=(elem: K): State[S, Set[K]]
    = lens %= (_ - elem)

    def -=(elem1: K, elem2: K, elems: K*): State[S, Set[K]] =
      lens %= (_ - elem1 - elem2 -- elems)

    def --=(xs: TraversableOnce[K]): State[S, Set[K]] =
      lens %= (_ -- xs)
  }

  /**A lens that views an immutable Map type can provide a mutable.Map-like API via State */
  case class MapLens[S, K, V](lens: S @@ Map[K, V]) {
    /**Allows both viewing and setting the value of a member of the map */
    def member(k: K): (S @@ Option[V]) = lensG[S, Option[V]](
      s => lens.get(s).get(k),
      s => opt => lens.mod(m => opt match {
        case Some(v) => m + (k -> v)
        case None => m - k
      })(s))

    /**This lens has undefined behavior when accessing an element not present in the map! */
    def at(k: K): (S @@ V) =
      lensG[S, V](lens.get(_)(k), s => v => lens.mod(_ + (k -> v))(s))

    def +=(elem1: (K, V), elem2: (K, V), elems: (K, V)*): State[S, Map[K, V]] =
      lens %= (_ + elem1 + elem2 ++ elems)

    def +=(elem: (K, V)): State[S, Map[K, V]] =
      lens %= (_ + elem)

    def ++=(xs: TraversableOnce[(K, V)]): State[S, Map[K, V]] =
      lens %= (_ ++ xs)

    def update(key: K, value: V): State[S, Unit] =
      lens %== (_.updated(key, value))

    def -=(elem: K): State[S, Map[K, V]]
    = lens %= (_ - elem)

    def -=(elem1: K, elem2: K, elems: K*): State[S, Map[K, V]] =
      lens %= (_ - elem1 - elem2 -- elems)

    def --=(xs: TraversableOnce[K]): State[S, Map[K, V]] =
      lens %= (_ -- xs)
  }

  implicit def mapLens[S, K, V](lens: S @@ Map[K, V]) = MapLens[S, K, V](lens)

  /**Provide the appearance of a mutable-like API for sorting sequences through a lens */
  case class SeqLikeLens[S, A, Repr <: SeqLike[A, Repr]](lens: S @@ Repr) {
    def sortWith(lt: (A, A) => Boolean): State[S, Unit]
    = lens %== (_ sortWith lt)

    def sortBy[B: math.Ordering](f: A => B): State[S, Unit]
    = lens %== (_ sortBy f)

    def sort[B >: A](implicit ord: math.Ordering[B]) =
      lens %== (_.sorted[B]): State[S, Unit]
  }

  implicit def seqLikeLens[S, A, Repr <: SeqLike[A, Repr]](lens: S @@ Repr) =
    SeqLikeLens[S, A, Repr](lens)

  implicit def seqLens[S, A](lens: Lens[S, scala.collection.immutable.Seq[A]]) =
    seqLikeLens[S, A, scala.collection.immutable.Seq[A]](lens)

  /**Provide an imperative-seeming API for stacks viewed through a lens */
  case class StackLens[S, A](lens: S @@ Stack[A]) {
    def push(elem1: A, elem2: A, elems: A*): State[S, Unit] =
      lens %== (_ push elem1 push elem2 pushAll elems)

    def push1(elem: A): State[S, Unit] =
      lens %== (_ push elem)

    def pop: State[S, Unit] =
      lens %== (_ pop)

    def pop2: State[S, A] =
      lens %%= (state(_.pop2))

    def top: State[S, A] =
      lens >- (_.top)

    def length: State[S, Int] =
      lens >- (_.length)
  }

  implicit def stackLens[S, A](lens: S @@ Stack[A]) =
    StackLens[S, A](lens)


  import collection.immutable.Queue

  /**Provide an imperative-seeming API for queues viewed through a lens */
  case class QueueLens[S, A](lens: S @@ Queue[A]) {
    def enqueue(elem: A): State[S, Unit] =
      lens %== (_ enqueue elem)

    def dequeue: State[S, A] =
      lens %%= (state(_.dequeue))

    def length: State[S, Int] =
      lens >- (_.length)
  }

  implicit def queueLens[S, A](lens: S @@ Queue[A]) =
    QueueLens[S, A](lens)

  /**Provide an imperative-seeming API for arrays viewed through a lens */
  case class ArrayLens[S, A](lens: S @@ Array[A]) {
    def at(n: Int): (S @@ A) =
      lensG[S, A](
        s => lens.get(s)(n),
        s => v => lens.mod(array => {
          val copy = array.clone()
          copy.update(n, v)
          copy
        })(s)
      )

    def length: State[S, Int] =
      lens >- (_.length)
  }

  implicit def arrayLens[S, A](lens: S @@ Array[A]) =
    ArrayLens[S, A](lens)

  /**Allow the illusion of imperative updates to numbers viewed through a lens */
  case class NumericLens[S, N: Numeric](lens: S @@ N, num: Numeric[N]) {
    def +=(that: N): State[S, N] =
      lens %= (num.minus(_, that))

    def -=(that: N): State[S, N] =
      lens %= (num.minus(_, that))

    def *=(that: N): State[S, N] =
      lens %= (num.times(_, that))
  }

  implicit def numericLens[S, N: Numeric](lens: S @@ N) =
    NumericLens[S, N](lens, implicitly[Numeric[N]])

  /**Allow the illusion of imperative updates to numbers viewed through a lens */
  case class FractionalLens[S, F](lens: S @@ F, frac: Fractional[F]) {
    def /=(that: F): State[S, F] =
      lens %= (frac.div(_, that))
  }

  implicit def fractionalLens[S, F: Fractional](lens: S @@ F) =
    FractionalLens[S, F](lens, implicitly[Fractional[F]])

  /**Allow the illusion of imperative updates to numbers viewed through a lens */
  case class IntegralLens[S, I](lens: S @@ I, ig: Integral[I]) {
    def %=(that: I): State[S, I] =
      lens %= (ig.quot(_, that))
  }

  implicit def integralLens[S, I: Integral](lens: S @@ I) =
    IntegralLens[S, I](lens, implicitly[Integral[I]])
}
