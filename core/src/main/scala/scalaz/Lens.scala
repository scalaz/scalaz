package scalaz

import StoreT._
import Id._

/**
 * A Lens, offering a purely functional means to access and retrieve
 * a field of type `B` in a record of type `A`.
 *
 * The term ''field'' should not be interpreted restrictively to mean a member of a class. For example, a lens
 * can address membership of a `Set`.
 *
 * @see [[scalaz.PLens]]
 *
 * @tparam A The type of the record
 * @tparam B The type of the field
 */
sealed trait LensT[F[+_], A, B] {
  def run(a: A): F[Store[B, A]]

  def apply(a: A): F[Store[B, A]] =
    run(a)

  import StateT._
  import LensT._
  import BijectionT._
  import WriterT._

  def xmapA[X](f: A => X)(g: X => A)(implicit F: Functor[F]): LensT[F, X, B] =
    lensT(x => F.map(run(g(x)))(_ map (f)))

  def xmapbA[X](b: Bijection[A, X])(implicit F: Functor[F]): LensT[F, X, B] =
    xmapA(b to _)(b from _)

  def xmapB[X](f: B => X)(g: X => B)(implicit F: Functor[F]): LensT[F, A, X] =
    lensT(a => F.map(run(a))(_.xmap(f)(g)))

  def xmapbB[X](b: Bijection[B, X])(implicit F: Functor[F]): LensT[F, A, X] =
    xmapB(b to _)(b from _)

  def get(a: A)(implicit F: Functor[F]): F[B] =
    F.map(run(a))(_.pos)

  def set(a: A, b: B)(implicit F: Functor[F]): F[A] =
    F.map(run(a))(_.put(b))

  def st(implicit F: Functor[F]): StateT[F, A, B] =
    StateT(s => F.map(get(s))((s, _)))

  /** Modify the value viewed through the lens */
  def mod(f: B => B, a: A)(implicit F: Functor[F]): F[A] =
    F.map(run(a))(c => {
      val (p, q) = c.run
      p(f(q))
    })

  def =>=(f: B => B)(implicit F: Functor[F]): A => F[A] =
    mod(f, _)

  /** Modify the value viewed through the lens, returning a functor `X` full of results. */
  def modf[X[_]](f: B => X[B], a: A)(implicit F: Functor[F], XF: Functor[X]): F[X[A]] =
    F.map(run(a))(c => XF.map(f(c.pos))(c put _))

  def =>>=[X[_]](f: B => X[B])(implicit F: Functor[F], XF: Functor[X]): A => F[X[A]] =
    modf(f, _)

  /** Modify the value viewed through the lens, returning a `C` on the side.  */
  def modp[C](f: B => F[(B, C)], a: A)(implicit F: Bind[F]): F[(A, C)] = F.bind(
    get(a))(x => F.bind(
    f(x)){
      case (b, c) => F.map(set(a, b))((_, c))
    })

  /** Modify the portion of the state viewed through the lens and return its new value. */
  def mods(f: B => B)(implicit F: Functor[F]): StateT[F, A, B] =
    StateT(a =>
      F.map(run(a))(c => {
        val b = f(c.pos)
        (c put b, b)
      }))

  /** Modify the portion of the state viewed through the lens and return its new value. */
  def %=(f: B => B)(implicit F: Functor[F]): StateT[F, A, B] =
    mods(f)

  /** Set the portion of the state viewed through the lens and return its new value. */
  def assign(b: => B)(implicit F: Functor[F]): StateT[F, A, B] =
    mods(_ => b)

  /** Set the portion of the state viewed through the lens and return its new value. */
  def :=(b: => B)(implicit F: Functor[F]): StateT[F, A, B] =
    assign(b)

  /** Modify the portion of the state viewed through the lens, but do not return its new value. */
  def mods_(f: B => B)(implicit F: Functor[F]): StateT[F, A, Unit] =
    StateT(a =>
      F.map(mod(f, a))((_, ())))

  /** Modify the portion of the state viewed through the lens, but do not return its new value. */
  def %==(f: B => B)(implicit F: Functor[F]): StateT[F, A, Unit] =
    mods_(f)

  /** Contravariantly map a state action through a lens. */
  def lifts[C](s: StateT[F, B, C])(implicit M: Bind[F]): StateT[F, A, C] =
    StateT(a => modp(s(_), a))

  def %%=[C](s: StateT[F, B, C])(implicit M: Bind[F]): StateT[F, A, C] =
    lifts(s)

  /** Map the function `f` over the lens as a state action. */
  def map[C](f: B => C)(implicit F: Functor[F]): StateT[F, A, C] =
    StateT(a => F.map(get(a))(x => (a, f(x))))

  /** Map the function `f` over the value under the lens, as a state action. */
  def >-[C](f: B => C)(implicit F: Functor[F]): StateT[F, A, C] = map(f)

  /** Bind the function `f` over the value under the lens, as a state action. */
  def flatMap[C](f: B => StateT[F, A, C])(implicit F: Bind[F]): StateT[F, A, C] =
    StateT(a => F.bind(get(a))(x => f(x)(a)))

  /** Bind the function `f` over the value under the lens, as a state action. */
  def >>-[C](f: B => StateT[F, A, C])(implicit F: Bind[F]): StateT[F, A, C] = flatMap(f)

  /** Sequence the monadic action of looking through the lens to occur before the state action `f`. */
  def ->>-[C](f: => StateT[F, A, C])(implicit F: Bind[F]): StateT[F, A, C] =
    >>-(_ => f)

  /** Contravariantly mapping the state of a state monad through a lens is a natural transformation */
  def liftsNT(implicit F: Bind[F]): ({type m[x] = StateT[F,B,x]})#m ~> ({type n[x] = StateT[F,A,x]})#n =
    new (({type m[x] = StateT[F,B,x]})#m ~> ({type n[x] = StateT[F,A,x]})#n) {
      def apply[C](s : StateT[F,B,C]): StateT[F,A,C] = StateT[F,A,C](a => modp(s(_), a))
    }

  /** Lenses can be composed */
  def compose[C](that: LensT[F, C, A])(implicit F: Bind[F]): LensT[F, C, B] =
    lensT(c =>
      F.bind(that run c)(x => {
        val (ac, a) = x.run
        F.map(run(a))(y => {
          val (ba, b) = y.run
          Store(ac compose ba, b)
        })
      }))

  /** alias for `compose` */
  def <=<[C](that: LensT[F, C, A])(implicit F: Bind[F]): LensT[F, C, B] = compose(that)

  def andThen[C](that: LensT[F, B, C])(implicit F: Bind[F]): LensT[F, A, C] =
    that compose this

  /** alias for `andThen` */
  def >=>[C](that: LensT[F, B, C])(implicit F: Bind[F]): LensT[F, A, C] = andThen(that)

  /** Two lenses that view a value of the same type can be joined */
  def sum[C](that: => LensT[F, C, B])(implicit F: Functor[F]): LensT[F, A \/ C, B] =
    lensT{
      case -\/(a) =>
        F.map(run(a))(_ map (-\/(_)))
      case \/-(c) =>
        F.map(that run c)(_ map (\/-(_)))
    }

  /** Alias for `sum` */
  def |||[C](that: => LensT[F, C, B])(implicit F: Functor[F]): LensT[F, A \/ C, B] = sum(that)

  /** Two disjoint lenses can be paired */
  def product[C, D](that: LensT[F, C, D])(implicit F: Apply[F]): LensT[F, (A, C), (B, D)] =
    lensT {
      case (a, c) => F.apply2(run(a), that run c)((x, y) => x *** y)
    }

  /** alias for `product` */
  def ***[C, D](that: LensT[F, C, D])(implicit F: Apply[F]): LensT[F, (A, C), (B, D)] = product(that)

  trait LensLaw {
    def identity(a: A)(implicit A: Equal[A], ev: F[Store[B, A]] =:= Id[Store[B, A]]): Boolean = {
      val c = run(a)
      A.equal(c.put(c.pos), a)
    }
    def retention(a: A, b: B)(implicit B: Equal[B], ev: F[Store[B, A]] =:= Id[Store[B, A]]): Boolean =
      B.equal(run(run(a) put b).pos, b)
    def doubleSet(a: A, b1: B, b2: B)(implicit A: Equal[A], ev: F[Store[B, A]] =:= Id[Store[B, A]]) = {
      val r = run(a)
      A.equal(run(r put b1) put b2, r put b2)
    }
  }

  def lensLaw = new LensLaw {}

  /** A homomorphism of lens categories */
  def partial(implicit F: Functor[F]): PLensT[F, A, B] =
    PLensT.plensT(a => F.map(run(a))(x => Some(x):Option[Store[B, A]]))

  /** alias for `partial` */
  def unary_~(implicit F: Functor[F]) : PLensT[F, A, B] =
    partial
}

object LensT extends LensTFunctions with LensTInstances {
  def apply[F[+_], A, B](r: A => F[Store[B, A]]): LensT[F, A, B] =
    lensT(r)
}

trait LensTFunctions {
  import StoreT._

  def lensT[F[+_], A, B](r: A => F[Store[B, A]]): LensT[F, A, B] = new LensT[F, A, B] {
    def run(a: A): F[Store[B, A]] = r(a)
  }

  def lens[A, B](r: A => Store[B, A]): Lens[A, B] =
    lensT[Id, A, B](r)

  def lensp[F[+_], A, B](r: A => Store[B, A])(implicit F: Pointed[F]): LensT[F, A, B] =
    lensT(a => F.point(r(a)))

  def lensgT[F[+_], A, B](set: A => F[B => A], get: A => F[B])(implicit M: Bind[F]): LensT[F, A, B] =
    lensT(a => M.apply2(set(a), get(a))(Store(_, _)))

  def lensg[A, B](set: A => B => A, get: A => B): Lens[A, B] =
    lensgT[Id, A, B](set, get)

  def lensu[A, B](set: (A, B) => A, get: A => B): Lens[A, B] =
    lensg(set.curried, get)

  /** The identity lens for a given object */
  def lensId[F[+_], A](implicit P: Pointed[F]): LensT[F, A, A] =
    lensp(Store(identity, _))

  /** The identity lens through the Id functor */
  def self[A]: Lens[A, A] = lensId[Id, A]

  /** The trivial lens that can retrieve Unit from anything */
  def trivialLens[F[+_], A](implicit P: Pointed[F]): LensT[F, A, Unit] =
    lensp[F, A, Unit](a => Store(_ => a, ()))

  /** A lens that discards the choice of right or left from disjunction */
  def codiagLens[F[+_]: Pointed, A]: LensT[F, A \/ A, A] =
    lensId[F, A] ||| lensId[F, A]

  /** Access the first field of a tuple */
  def firstLens[A, B]: (A, B) @> A =
    lens {
      case (a, b) => Store(x => (x, b), a)
    }

  /** Access the second field of a tuple */
  def secondLens[A, B]: (A, B) @> B =
    lens {
      case (a, b) => Store(x => (a, x), b)
    }

  /** Access the first field of a tuple */
  def lazyFirstLens[A, B]: LazyTuple2[A, B] @> A =
    lens(z => Store(x => LazyTuple2(x, z._2), z._1))

  /** Access the second field of a tuple */
  def lazySecondLens[A, B]: LazyTuple2[A, B] @> B =
    lens(z => Store(x => LazyTuple2(z._1, x), z._2))

  def nelHeadLens[A]: NonEmptyList[A] @> A =
    lens(l => Store(NonEmptyList.nel(_, l.tail), l.head))

  def nelTailLens[A]: NonEmptyList[A] @> List[A] =
    lens(l => Store(NonEmptyList.nel(l.head, _), l.tail))

  /** Access the value at a particular key of a Map **/
  def mapVLens[K, V](k: K): Map[K, V] @> Option[V] =
    lensg(m => ({
      case None => m - k
      case Some(v) => m.updated(k, v)
    }: Option[V] => Map[K, V]), _ get k)

  def applyLens[A, B](k: B => A)(implicit e: Equal[A]): Store[A, B] @> B =
    lens(q => {
      lazy val x = q.pos
      lazy val y = q put x
      Store(b =>
        Store(w => if(e equal (x, w)) b else y, x), y)
    })

  def predicateLens[A]: Store[A, Boolean] @> (A \/ A) =
    Lens(q => Store(_ match {
      case -\/(l) => Store(_ => true, l)
      case \/-(r) => Store(_ => false, r)
    }, {
      val x = q.pos
      if(q put x) -\/(x) else \/-(x)
    }))

  def factorLens[A, B, C]: ((A, B) \/ (A, C)) @> (A, B \/ C) =
    lens(e => Store({
      case (a, -\/(b)) => -\/(a, b)
      case (a, \/-(c)) => \/-(a, c)
    }, e match {
      case -\/((a, b)) => (a, -\/(b))
      case \/-((a, c)) => (a, \/-(c))
    }))

  def distributeLens[A, B, C]: (A, B \/ C) @> ((A, B) \/ (A, C)) =
    lens {
      case (a, e) => Store({
        case -\/((aa, bb)) => (aa, -\/(bb))
        case \/-((aa, cc)) => (aa, \/-(cc))
      }, e match {
        case -\/(b) => -\/(a, b)
        case \/-(c) => \/-(a, c)

      })
    }

}

trait LensTInstances0 {
  implicit def lensTArrId[F[+_]](implicit F0: Pointed[F]): ArrId[({type λ[α, β] = LensT[F, α, β]})#λ] = new LensTArrId[F] {
    implicit def F = F0
  }
}

trait LensTInstances {
  import LensT._
  import collection.immutable.Stack
  import collection.SeqLike
  import collection.immutable.Queue
  import BijectionT._

  implicit def lensTCategory[F[+_]](implicit F0: Monad[F]) = new LensTCategory[F] {
    implicit def F = F0
  }

  /** Lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def LensState[F[+_], A, B](lens: LensT[F, A, B])(implicit F: Functor[F]): StateT[F, A, B] =
    lens.st

  implicit def LensTUnzip[F[+_], S](implicit F: Functor[F]): Unzip[({type λ[α] = LensT[F, S, α]})#λ] =
    new Unzip[({type λ[α] = LensT[F, S, α]})#λ] {
      def unzip[A, B](a: LensT[F, S, (A, B)]) =
        (
          LensT(x => F.map(a run x)(c => {
            val (p, q) = c.pos
            Store(a => c.put((a, q)): S, p)
          }))
          , LensT(x => F.map(a run x)(c => {
          val (p, q) = c.pos
          Store(a => c.put((p, a)): S, q)
        }))
        )
    }

  case class SetLens[S, K](lens: Lens[S, Set[K]]) {
    /** Setting the value of this lens will change whether or not it is present in the set */
    def contains(key: K) = LensT.lensg[S, Boolean](
      s => b => lens.mod(m => if (b) m + key else m - key, s): Id[S]
    , s => lens.get(s).contains(key)
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

  /** A lens that views a Set can provide the appearance of in place mutation */
  implicit def setLens[S, K](lens: Lens[S, Set[K]]) =
    SetLens[S, K](lens)

  /** A lens that views an immutable Map type can provide a mutable.Map-like API via State */
  case class MapLens[S, K, V](lens: Lens[S, Map[K, V]]) {
    /** Allows both viewing and setting the value of a member of the map */
    def member(k: K): Lens[S, Option[V]] = lensg[S, Option[V]](
      s => opt => lens.mod((m: Map[K, V]) => (opt match {
        case Some(v) => m + (k -> v)
        case None    => m - k
      }): Map[K, V], s): Id[S]
      , s => lens.get(s).get(k))

    /** This lens has undefined behavior when accessing an element not present in the map! */
    def at(k: K): Lens[S, V] =
      lensg[S, V](s => v => lens.mod(_ + (k -> v), s): Id[S], lens.get(_) apply k)

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

  implicit def mapLens[S, K, V](lens: Lens[S, Map[K, V]]) =
    MapLens[S, K, V](lens)

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
      lens %== (_.pop)

    def pop2: State[S, A] =
      lens %%= State(_.pop2.swap)

    def top: State[S, A] =
      lens >- (_.top)

    def length: State[S, Int] =
      lens >- (_.length)
  }

  implicit def stackLens[S, A](lens: Lens[S, Stack[A]]) =
    StackLens[S, A](lens)

  /** Provide an imperative-seeming API for queues viewed through a lens */
  case class QueueLens[S, A](lens: Lens[S, Queue[A]]) {
    def enqueue(elem: A): State[S, Unit] =
      lens %== (_ enqueue elem)

    def dequeue: State[S, A] =
      lens %%= (State(_.dequeue.swap))

    def length: State[S, Int] =
      lens >- (_.length)
  }

  implicit def queueLens[S, A](lens: Lens[S, Queue[A]]) =
    QueueLens[S, A](lens)

  /** Provide an imperative-seeming API for arrays viewed through a lens */
  case class ArrayLens[S, A](lens: Lens[S, Array[A]]) {
    def at(n: Int): (Lens[S, A]) =
      lensg[S, A](
        s => v => lens.mod(array => {
          val copy = array.clone()
          copy.update(n, v)
          copy
        }, s): Id[S]
        , s => lens.get(s) apply n
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

  implicit def tuple2Lens[F[+_]: Functor, S, A, B](lens: LensT[F, S, (A, B)]):
  (LensT[F, S, A], LensT[F, S, B]) =
    LensTUnzip[F, S].unzip(lens)

  implicit def tuple3Lens[F[+_]: Functor, S, A, B, C](lens: LensT[F, S, (A, B, C)]):
  (LensT[F, S, A], LensT[F, S, B], LensT[F, S, C]) =
    LensTUnzip[F, S].unzip3(lens.xmapbB(tuple3B))

  implicit def tuple4Lens[F[+_]: Functor, S, A, B, C, D](lens: LensT[F, S, (A, B, C, D)]):
  (LensT[F, S, A], LensT[F, S, B], LensT[F, S, C], LensT[F, S, D]) =
    LensTUnzip[F, S].unzip4(lens.xmapbB(tuple4B))

  implicit def tuple5Lens[F[+_]: Functor, S, A, B, C, D, E](lens: LensT[F, S, (A, B, C, D, E)]):
  (LensT[F, S, A], LensT[F, S, B], LensT[F, S, C], LensT[F, S, D], LensT[F, S, E]) =
    LensTUnzip[F, S].unzip5(lens.xmapbB(tuple5B))

  implicit def tuple6Lens[F[+_]: Functor, S, A, B, C, D, E, H](lens: LensT[F, S, (A, B, C, D, E, H)]):
  (LensT[F, S, A], LensT[F, S, B], LensT[F, S, C], LensT[F, S, D], LensT[F, S, E], LensT[F, S, H]) =
    LensTUnzip[F, S].unzip6(lens.xmapbB(tuple6B))

  implicit def tuple7Lens[F[+_]: Functor, S, A, B, C, D, E, H, I](lens: LensT[F, S, (A, B, C, D, E, H, I)]):
  (LensT[F, S, A], LensT[F, S, B], LensT[F, S, C], LensT[F, S, D], LensT[F, S, E], LensT[F, S, H], LensT[F, S, I]) =
    LensTUnzip[F, S].unzip7(lens.xmapbB(tuple7B))
}

private[scalaz] trait LensTArrId[F[+_]]
  extends ArrId[({type λ[α, β] = LensT[F, α, β]})#λ]{

  implicit def F: Pointed[F]

  def id[A] = LensT.lensId
}

private[scalaz] trait LensTCategory[F[+_]]
  extends Choice[({type λ[α, β] = LensT[F, α, β]})#λ]
  with Split[({type λ[α, β] = LensT[F, α, β]})#λ]
  with LensTArrId[F] {

  implicit def F: Monad[F]

  def compose[A, B, C](bc: LensT[F, B, C], ab: LensT[F, A, B]): LensT[F, A, C] = ab >=> bc

  def choice[A, B, C](f: => LensT[F, A, C], g: => LensT[F, B, C]): LensT[F, A \/ B, C] =
    LensT.lensT {
      case -\/(a) =>
        F.map(f run a)(_ map (-\/(_)))
      case \/-(b) =>
        F.map(g run b)(_ map (\/-(_)))
    }

  def split[A, B, C, D](f: LensT[F, A, B], g: LensT[F, C, D]): LensT[F, (A,  C), (B, D)] =
    f *** g

}
