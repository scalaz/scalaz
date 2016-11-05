package scalaz

import Id._

/**
 * A Lens Family, offering a purely functional means to access and retrieve
 * a field transitioning from type `B1` to type `B2` in a record simultaneously
 * transitioning from type `A1` to type `A2`.  [[scalaz.Lens]] is a convenient
 * alias for when `A1 =:= A2`, and `B1 =:= B2`.
 *
 * The term ''field'' should not be interpreted restrictively to mean a member of a class. For example, a lens
 * family can address membership of a `Set`.
 *
 * @see [[scalaz.PLens]]
 *
 * @tparam A1 The initial type of the record
 * @tparam A2 The final type of the record
 * @tparam B1 The initial type of the field
 * @tparam B2 The final type of the field
 */
sealed abstract class LensFamily[A1, A2, B1, B2] {
  def run(a: A1): IndexedStore[B1, B2, A2]

  def apply(a: A1): IndexedStore[B1, B2, A2] =
    run(a)

  import LensFamily._
  import BijectionT._

  def xmapA[X1, X2](f: A2 => X2)(g: X1 => A1): LensFamily[X1, X2, B1, B2] =
    lensFamily(x => run(g(x)) map f)

  def xmapbA[X, A >: A2 <: A1](b: Bijection[A, X]): LensFamily[X, X, B1, B2] =
    xmapA(b to _)(b from _)

  def xmapB[X1, X2](f: B1 => X1)(g: X2 => B2): LensFamily[A1, A2, X1, X2] =
    lensFamily(a => run(a).xmap(f)(g))

  def xmapbB[X, B >: B1 <: B2](b: Bijection[B, X]): LensFamily[A1, A2, X, X] =
    xmapB(b to _)(b from _)

  def get(a: A1): B1 =
    run(a).pos

  def set(a: A1, b: B2): A2 =
    run(a).put(b)

  def st: State[A1, B1] =
    State(s => (s, get(s)))

  /** Modify the value viewed through the lens */
  def mod(f: B1 => B2, a: A1): A2 = {
    val (p, q) = run(a).run
    p(f(q))
  }

  def =>=(f: B1 => B2): A1 => A2 =
    mod(f, _)

  /** Modify the value viewed through the lens, returning a functor `X` full of results. */
  def modf[X[_]](f: B1 => X[B2], a: A1)(implicit XF: Functor[X]): X[A2] = {
    val c = run(a)
    XF.map(f(c.pos))(c put _)
  }

  def =>>=[X[_]](f: B1 => X[B2])(implicit XF: Functor[X]): A1 => X[A2] =
    modf(f, _)

  /** Modify the value viewed through the lens, returning a `C` on the side.  */
  def modp[C](f: B1 => (B2, C), a: A1): (A2, C) = {
    val (b, c) = f(get(a))
    (set(a, b), c)
  }

  /** Modify the portion of the state viewed through the lens and return its new value. */
  def mods(f: B1 => B2): IndexedState[A1, A2, B2] =
    IndexedState(a => {
      val c = run(a)
      val b = f(c.pos)
      (c put b, b)
    })

  /** Modify the portion of the state viewed through the lens and return its new value. */
  def %=(f: B1 => B2): IndexedState[A1, A2, B2] =
    mods(f)

  /** Modify the portion of the state viewed through the lens and return its old value.
   * @since 7.0.2
   */
  def modo(f: B1 => B2): IndexedState[A1, A2, B1] =
    IndexedState(a => {
      val c = run(a)
      val o = c.pos
      (c put f(o), o)
    })

  /** Modify the portion of the state viewed through the lens and return its old value. alias for `modo`
   * @since 7.0.2
   */
  def <%=(f: B1 => B2): IndexedState[A1, A2, B1] =
    modo(f)

  /** Set the portion of the state viewed through the lens and return its new value. */
  def assign(b: => B2): IndexedState[A1, A2, B2] =
    mods(_ => b)

  /** Set the portion of the state viewed through the lens and return its new value. */
  def :=(b: => B2): IndexedState[A1, A2, B2] =
    assign(b)

  /** Set the portion of the state viewed through the lens and return its old value.
   * @since 7.0.2
   */
  def assigno(b: => B2): IndexedState[A1, A2, B1] =
    modo(_ => b)

  /** Set the portion of the state viewed through the lens and return its old value. alias for `assigno`
   * @since 7.0.2
   */
  def <:=(b: => B2): IndexedState[A1, A2, B1] =
    assigno(b)

  /** Modify the portion of the state viewed through the lens, but do not return its new value. */
  def mods_(f: B1 => B2): IndexedState[A1, A2, Unit] =
    IndexedState(a =>
      (mod(f, a), ()))

  /** Modify the portion of the state viewed through the lens, but do not return its new value. */
  def %==(f: B1 => B2): IndexedState[A1, A2, Unit] =
    mods_(f)

  /** Contravariantly map a state action through a lens. */
  def lifts[C](s: IndexedState[B1, B2, C]): IndexedState[A1, A2, C] =
    IndexedState(a => modp(s(_), a))

  def %%=[C](s: IndexedState[B1, B2, C]): IndexedState[A1, A2, C] =
    lifts(s)

  /** Map the function `f` over the lens as a state action. */
  def map[C](f: B1 => C): State[A1, C] =
    State(a => (a, f(get(a))))

  /** Map the function `f` over the value under the lens, as a state action. */
  def >-[C](f: B1 => C): State[A1, C] = map(f)

  /** Bind the function `f` over the value under the lens, as a state action. */
  def flatMap[C](f: B1 => IndexedState[A1, A2, C]): IndexedState[A1, A2, C] =
    IndexedState(a => f(get(a))(a))

  /** Bind the function `f` over the value under the lens, as a state action. */
  def >>-[C](f: B1 => IndexedState[A1, A2, C]): IndexedState[A1, A2, C] =
    flatMap[C](f)

  /** Sequence the monadic action of looking through the lens to occur before the state action `f`. */
  def ->>-[C](f: => IndexedState[A1, A2, C]): IndexedState[A1, A2, C] =
    flatMap(_ => f)

  /** Contravariantly mapping the state of a state monad through a lens is a natural transformation */
  def liftsNT: IndexedState[B1, B2, ?] ~> IndexedState[A1, A2, ?] =
    λ[IndexedState[B1, B2, ?] ~> IndexedState[A1, A2, ?]](
      s => IndexedState(a => modp(s(_), a))
    )

  /** Lenses can be composed */
  def compose[C1, C2](that: LensFamily[C1, C2, A1, A2]): LensFamily[C1, C2, B1, B2] =
    lensFamily(c => {
      val (ac, a) = that.run(c).run
      val (ba, b) = run(a).run
      IndexedStore(ac compose ba, b)
    })

  /** alias for `compose` */
  def <=<[C1, C2](that: LensFamily[C1, C2, A1, A2]): LensFamily[C1, C2, B1, B2] = compose(that)

  def andThen[C1, C2](that: LensFamily[B1, B2, C1, C2]): LensFamily[A1, A2, C1, C2] =
    that compose this

  /** alias for `andThen` */
  def >=>[C1, C2](that: LensFamily[B1, B2, C1, C2]): LensFamily[A1, A2, C1, C2] = andThen(that)

  /** Two lenses that view a value of the same type can be joined */
  def sum[C1, C2](that: => LensFamily[C1, C2, B1, B2]): LensFamily[A1 \/ C1, A2 \/ C2, B1, B2] =
    lensFamily{
      case -\/(a) =>
        run(a) map  (\/.left)
      case \/-(c) =>
        that run c map (\/.right)
    }

  /** Alias for `sum` */
  def |||[C1, C2](that: => LensFamily[C1, C2, B1, B2]): LensFamily[A1 \/ C1, A2 \/ C2, B1, B2] = sum(that)

  /** Two disjoint lenses can be paired */
  def product[C1, C2, D1, D2](that: LensFamily[C1, C2, D1, D2]): LensFamily[(A1, C1), (A2, C2), (B1, D1), (B2, D2)] =
    lensFamily {
      case (a, c) => run(a) *** that.run(c)
    }

  /** alias for `product` */
  def ***[C1, C2, D1, D2](that: LensFamily[ C1, C2, D1, D2]): LensFamily[(A1, C1), (A2, C2), (B1, D1), (B2, D2)] = product(that)

  trait LensLaw {
    def identity[A >: A2 <: A1, B >: B1 <: B2](a: A)(implicit A: Equal[A]): Boolean = {
      val c = run(a)
      A.equal(c.put(c.pos: B), a)
    }
    def retention[A >: A2 <: A1, B >: B1 <: B2](a: A, b: B)(implicit B: Equal[B]): Boolean =
      B.equal(run(run(a).put(b): A).pos, b)
    def doubleSet[A >: A2 <: A1, B >: B1 <: B2](a: A, b1: B, b2: B)(implicit A: Equal[A]): Boolean = {
      val r = run(a)
      A.equal(run(r.put(b1): A) put b2, r put b2)
    }
  }

  def lensLaw = new LensLaw {}

  /** A homomorphism of lens categories */
  def partial: PLensFamily[A1, A2, B1, B2] =
    PLensFamily.plensFamily(a => Some(run(a)):Option[IndexedStore[B1, B2, A2]])

  /** alias for `partial` */
  def unary_~ : PLensFamily[A1, A2, B1, B2] =
    partial

}

object LensFamily extends LensInstances with LensFunctions {
  def apply[A1, A2, B1, B2](r: A1 => IndexedStore[B1, B2, A2]): LensFamily[A1, A2, B1, B2] =
    lensFamily(r)
}

trait LensFamilyFunctions {

  def lensFamily[A1, A2, B1, B2](r: A1 => IndexedStore[B1, B2, A2]): LensFamily[A1, A2, B1, B2] = new LensFamily[A1, A2, B1, B2] {
    def run(a: A1): IndexedStore[B1, B2, A2] = r(a)
  }

  def lensFamilyg[A1, A2, B1, B2](set: A1 => B2 => A2, get: A1 => B1): LensFamily[A1, A2, B1, B2] =
    lensFamily(a => IndexedStore(set(a), get(a)))

  def lensFamilyu[A1, A2, B1, B2](set: (A1, B2) => A2, get: A1 => B1): LensFamily[A1, A2, B1, B2] =
    lensFamilyg(set.curried, get)

  /** The identity lens family for a given pair of objects */
  def lensFamilyId[A1, A2]: LensFamily[A1, A2, A1, A2] =
    lensFamily(IndexedStore(identity, _))

  /** A lens family that discards the choice of right or left from disjunction */
  def codiagLensFamily[A1, A2]: LensFamily[A1 \/ A1, A2 \/ A2, A1, A2] =
    lensFamilyId[A1, A2] ||| lensFamilyId[A1, A2]

  /** Polymorphically access the first field of a tuple */
  def firstLensFamily[A1, A2, B]: LensFamily[(A1, B), (A2, B), A1, A2] =
    lensFamily {
      case (a, b) => IndexedStore(x => (x, b), a)
    }

  /** Polymorphically access the second field of a tuple */
  def secondLensFamily[A, B1, B2]: LensFamily[(A, B1), (A, B2), B1, B2] =
    lensFamily {
      case (a, b) => IndexedStore(x => (a, x), b)
    }

  /** Polymorphically access the first field of a tuple */
  def lazyFirstLensFamily[A1, A2, B]: LensFamily[LazyTuple2[A1, B], LazyTuple2[A2, B], A1, A2] =
    lensFamily(z => IndexedStore(x => LazyTuple2(x, z._2), z._1))

  /** Polymorphically access the second field of a tuple */
  def lazySecondLensFamily[A, B1, B2]: LensFamily[LazyTuple2[A, B1], LazyTuple2[A, B2], B1, B2] =
    lensFamily(z => IndexedStore(x => LazyTuple2(z._1, x), z._2))

  def predicateLensFamily[A1, A2]: LensFamily[Store[A1, Boolean], Store[A2, Boolean], (A1 \/ A1), (A2 \/ A2)] =
    lensFamily(q => IndexedStore(_ match {
      case -\/(l) => Store(_ => true, l)
      case \/-(r) => Store(_ => false, r)
    }, {
      val x = q.pos
      if(q put x) -\/(x) else \/-(x)
    }))

  def factorLensFamily[A1, A2, B1, B2, C1, C2]: LensFamily[((A1, B1) \/ (A1, C1)), ((A2, B2) \/ (A2, C2)), (A1, B1 \/ C1), (A2, B2 \/ C2)] =
    lensFamily(e => IndexedStore({
      case (a, -\/(b)) => -\/(a, b)
      case (a, \/-(c)) => \/-(a, c)
    }, e match {
      case -\/((a, b)) => (a, -\/(b))
      case \/-((a, c)) => (a, \/-(c))
    }))

  def distributeLensFamily[A1, A2, B1, B2, C1, C2]: LensFamily[(A1, B1 \/ C1), (A2, B2 \/ C2), ((A1, B1) \/ (A1, C1)), ((A2, B2) \/ (A2, C2))] =
    lensFamily {
      case (a, e) => IndexedStore({
        case -\/((aa, bb)) => (aa, -\/(bb))
        case \/-((aa, cc)) => (aa, \/-(cc))
      }, e match {
        case -\/(b) => -\/(a, b)
        case \/-(c) => \/-(a, c)

      })
    }

}

trait LensFunctions extends LensFamilyFunctions {

  def lens[A, B](r: A => Store[B, A]): Lens[A, B] = new Lens[A, B] {
    def run(a: A): Store[B, A] = r(a)
  }

  def lensg[A, B](set: A => B => A, get: A => B): Lens[A, B] =
    lens(a => Store(set(a), get(a)))

  def lensu[A, B](set: (A, B) => A, get: A => B): Lens[A, B] =
    lensg(set.curried, get)

  /** The identity lens for a given object */
  def lensId[A]: Lens[A, A] =
    lens(Store(identity, _))

  /** The trivial lens that can retrieve Unit from anything */
  def trivialLens[A]: Lens[A, Unit] =
    lens[A, Unit](a => Store(_ => a, ()))

  /** A lens that discards the choice of right or left from disjunction */
  def codiagLens[A]: Lens[A \/ A, A] =
    lensId[A] ||| lensId[A]

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
    lens(l => Store(ll => NonEmptyList.nel(l.head, IList.fromList(ll)), l.tail.toList))

  /** Access the value at a particular key of a Map **/
  def mapVLens[K, V](k: K): Map[K, V] @> Option[V] =
    lensg(m => ({
      case None => m - k
      case Some(v) => m.updated(k, v)
    }: Option[V] => Map[K, V]), _ get k)

  /** Access the value at a particular key of a Map.WithDefault */
  def mapWithDefaultLens[K,V](k: K): Map.WithDefault[K,V] @> V =
    lensg(m => v => m.updated(k,v), m => m(k))

  /** Specify whether a value is in a Set */
  def setMembershipLens[A](a: A): Set[A] @> Boolean =
    lensg(s => b => if (b) s + a else s - a, _.contains(a))

  def applyLens[A, B](k: B => A)(implicit e: Equal[A]): Store[A, B] @> B =
    lens(q => {
      val x = Need(q.pos)
      val y = Need(q put x.value)
      Store(b =>
        Store(w => if(e equal (x.value, w)) b else y.value, x.value), y.value)
    })

  def predicateLens[A]: Store[A, Boolean] @> (A \/ A) =
    lens(q => Store(_ match {
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

sealed abstract class LensInstances0 { this: LensInstances =>
  import scala.collection.SeqLike

  implicit def seqLikeLensFamily[S1, S2, A, Repr <: SeqLike[A, Repr]](lens: LensFamily[S1, S2, Repr, Repr]) =
    SeqLikeLens[S1, S2, A, Repr](lens)

}

abstract class LensInstances extends LensInstances0 {
  import LensFamily._
  import BijectionT._
  import collection.SeqLike
  import collection.immutable.Queue

  implicit val lensCategory: LensCategory = new LensCategory {
  }

  /** Lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def LensFamilyState[A, B](lens: LensFamily[A, _, B, _]): State[A, B] =
    lens.st

  implicit def LensFamilyUnzip[S, R]: Unzip[λ[α => LensFamily[S, R, α, α]]] =
    new Unzip[λ[α => LensFamily[S, R, α, α]]] {
      def unzip[A, B](a: LensFamily[S, R, (A, B), (A, B)]) =
        (
          lensFamily(x => {
            val c = a run x
            val (p, q) = c.pos
            IndexedStore(a => c.put((a, q)): R, p)
          })
        , lensFamily(x => {
          val c = a run x
          val (p, q) = c.pos
          IndexedStore(a => c.put((p, a)): R, q)
        })
        )
    }

  type SetLens[S, K] = SetLensFamily[S, S, K]
  val SetLens: SetLensFamily.type = SetLensFamily
  case class SetLensFamily[S1, S2, K](lens: LensFamily[S1, S2, Set[K], Set[K]]) {
    /** Setting the value of this lens will change whether or not it is present in the set */
    def contains(key: K) = lensFamilyg[S1, S2, Boolean, Boolean](
      s => b => lens.mod(m => if (b) m + key else m - key, s): Id[S2]
    , s => lens.get(s).contains(key)
    )

    def &=(that: Set[K]): IndexedState[S1, S2, Set[K]] =
      lens %= (_ & that)

    def &~=(that: Set[K]): IndexedState[S1, S2, Set[K]] =
      lens %= (_ &~ that)

    def |=(that: Set[K]): IndexedState[S1, S2, Set[K]] =
      lens %= (_ | that)

    def +=(elem: K): IndexedState[S1, S2, Set[K]] =
      lens %= (_ + elem)

    def +=(elem1: K, elem2: K, elems: K*): IndexedState[S1, S2, Set[K]] =
      lens %= (_ + elem1 + elem2 ++ elems)

    def ++=(xs: TraversableOnce[K]): IndexedState[S1, S2, Set[K]] =
      lens %= (_ ++ xs)

    def -=(elem: K): IndexedState[S1, S2, Set[K]] =
      lens %= (_ - elem)

    def -=(elem1: K, elem2: K, elems: K*): IndexedState[S1, S2, Set[K]] =
      lens %= (_ - elem1 - elem2 -- elems)

    def --=(xs: TraversableOnce[K]): IndexedState[S1, S2, Set[K]] =
      lens %= (_ -- xs)
  }

  /** A lens that views a Set can provide the appearance of in place mutation */
  implicit def setLensFamily[S1, S2, K](lens: LensFamily[S1, S2, Set[K], Set[K]]) =
    SetLensFamily[S1, S2, K](lens)

  type MapLens[S, K, V] = MapLensFamily[S, S, K, V]
  val MapLens: MapLensFamily.type = MapLensFamily
  /** A lens that views an immutable Map type can provide a mutable.Map-like API via State */
  case class MapLensFamily[S1, S2, K, V](lens: LensFamily[S1, S2, Map[K, V], Map[K, V]]) {
    /** Allows both viewing and setting the value of a member of the map */
    def member(k: K): LensFamily[S1, S2, Option[V], Option[V]] = lensFamilyg[S1, S2, Option[V], Option[V]](
      s => opt => lens.mod((m: Map[K, V]) => (opt match {
        case Some(v) => m + (k -> v)
        case None    => m - k
      }): Map[K, V], s): Id[S2]
      , s => lens.get(s).get(k))

    /** This lens has undefined behavior when accessing an element not present in the map! */
    def at(k: K): LensFamily[S1, S2, V, V] =
      lensFamilyg[S1, S2, V, V](s => v => lens.mod(_ + (k -> v), s): Id[S2], lens.get(_) apply k)

    def +=(elem1: (K, V), elem2: (K, V), elems: (K, V)*): IndexedState[S1, S2, Map[K, V]] =
      lens %= (_ + elem1 + elem2 ++ elems)

    def +=(elem: (K, V)): IndexedState[S1, S2, Map[K, V]] =
      lens %= (_ + elem)

    def ++=(xs: TraversableOnce[(K, V)]): IndexedState[S1, S2, Map[K, V]] =
      lens %= (_ ++ xs)

    def update(key: K, value: V): IndexedState[S1, S2, Unit] =
      lens %== (_.updated(key, value))

    def -=(elem: K): IndexedState[S1, S2, Map[K, V]] =
      lens %= (_ - elem)

    def -=(elem1: K, elem2: K, elems: K*): IndexedState[S1, S2, Map[K, V]] =
      lens %= (_ - elem1 - elem2 -- elems)

    def --=(xs: TraversableOnce[K]): IndexedState[S1, S2, Map[K, V]] =
      lens %= (_ -- xs)
  }

  implicit def mapLensFamily[S1, S2, K, V](lens: LensFamily[S1, S2, Map[K, V], Map[K, V]]) =
    MapLensFamily[S1, S2, K, V](lens)

  type SeqLikeLens[S, A, Repr <: SeqLike[A, Repr]] = SeqLikeLensFamily[S, S, A, Repr]
  val SeqLikeLens: SeqLikeLensFamily.type = SeqLikeLensFamily
  /** Provide the appearance of a mutable-like API for sorting sequences through a lens */
  case class SeqLikeLensFamily[S1, S2, A, Repr <: SeqLike[A, Repr]](lens: LensFamily[S1, S2, Repr, Repr]) {
    def sortWith(lt: (A, A) => Boolean): IndexedState[S1, S2, Unit] =
      lens %== (_ sortWith lt)

    def sortBy[B: math.Ordering](f: A => B): IndexedState[S1, S2, Unit] =
      lens %== (_ sortBy f)

    def sort[B >: A](implicit ord: math.Ordering[B]): IndexedState[S1, S2, Unit] =
      lens %== (_.sorted[B])
  }

  implicit def seqLensFamily[S1, S2, A](lens: LensFamily[S1, S2, scala.collection.immutable.Seq[A], scala.collection.immutable.Seq[A]]) =
    seqLikeLensFamily[S1, S2, A, scala.collection.immutable.Seq[A]](lens)

  type QueueLens[S, A] = QueueLensFamily[S, S, A]
  val QueueLens: QueueLensFamily.type = QueueLensFamily
  /** Provide an imperative-seeming API for queues viewed through a lens */
  case class QueueLensFamily[S1, S2, A](lens: LensFamily[S1, S2, Queue[A], Queue[A]]) {
    def enqueue(elem: A): IndexedState[S1, S2, Unit] =
      lens %== (_ enqueue elem)

    def dequeue: IndexedState[S1, S2, A] =
      lens %%= State[Queue[A], A](_.dequeue.swap)

    def length: State[S1, Int] =
      lens >- (_.length)
  }

  implicit def queueLensFamily[S1, S2, A](lens: LensFamily[S1, S2, Queue[A], Queue[A]]) =
    QueueLensFamily[S1, S2, A](lens)

  type ArrayLens[S, A] = ArrayLensFamily[S, S, A]
  val ArrayLens: ArrayLensFamily.type = ArrayLensFamily
  /** Provide an imperative-seeming API for arrays viewed through a lens */
  case class ArrayLensFamily[S1, S2, A](lens: LensFamily[S1, S2, Array[A], Array[A]]) {
    def at(n: Int): LensFamily[S1, S2, A, A] =
      lensFamilyg[S1, S2, A, A](
        s => v => lens.mod(array => {
          val copy = array.clone()
          copy.update(n, v)
          copy
        }, s): Id[S2]
        , s => lens.get(s) apply n
      )

    def length: State[S1, Int] =
      lens >- (_.length)
  }

  implicit def arrayLensFamily[S1, S2, A](lens: LensFamily[S1, S2, Array[A], Array[A]]) =
    ArrayLensFamily[S1, S2, A](lens)

  type NumericLens[S, N] = NumericLensFamily[S, S, N]
  val NumericLens: NumericLensFamily.type = NumericLensFamily
  /** Allow the illusion of imperative updates to numbers viewed through a lens */
  case class NumericLensFamily[S1, S2, N](lens: LensFamily[S1, S2, N, N], num: Numeric[N]) {
    def +=(that: N): IndexedState[S1, S2, N] =
      lens %= (num.plus(_, that))

    def -=(that: N): IndexedState[S1, S2, N] =
      lens %= (num.minus(_, that))

    def *=(that: N): IndexedState[S1, S2, N] =
      lens %= (num.times(_, that))
  }

  implicit def numericLensFamily[S1, S2, N: Numeric](lens: LensFamily[S1, S2, N, N]) =
    NumericLens[S1, S2, N](lens, implicitly[Numeric[N]])

  type FractionalLens[S, F] = FractionalLensFamily[S, S, F]
  val FractionalLens: FractionalLensFamily.type = FractionalLensFamily
  /** Allow the illusion of imperative updates to numbers viewed through a lens */
  case class FractionalLensFamily[S1, S2, F](lens: LensFamily[S1, S2, F, F], frac: Fractional[F]) {
    def /=(that: F): IndexedState[S1, S2, F] =
      lens %= (frac.div(_, that))
  }

  implicit def fractionalLensFamily[S1, S2, F: Fractional](lens: LensFamily[S1, S2, F, F]) =
    FractionalLensFamily[S1, S2, F](lens, implicitly[Fractional[F]])

  type IntegralLens[S, I] = IntegralLensFamily[S, S, I]
  val IntegralLens: IntegralLensFamily.type = IntegralLensFamily
  /** Allow the illusion of imperative updates to numbers viewed through a lens */
  case class IntegralLensFamily[S1, S2, I](lens: LensFamily[S1, S2, I, I], ig: Integral[I]) {
    def %=(that: I): IndexedState[S1, S2, I] =
      lens %= (ig.quot(_, that))
  }

  implicit def integralLensFamily[S1, S2, I: Integral](lens: LensFamily[S1, S2, I, I]) =
    IntegralLensFamily[S1, S2, I](lens, implicitly[Integral[I]])

  implicit def tuple2LensFamily[S1, S2, A, B](lens: LensFamily[S1, S2, (A, B), (A, B)]):
  (LensFamily[S1, S2, A, A], LensFamily[S1, S2, B, B]) =
    LensFamilyUnzip[S1, S2].unzip(lens)

  implicit def tuple3LensFamily[S1, S2, A, B, C](lens: LensFamily[S1, S2, (A, B, C), (A, B, C)]):
  (LensFamily[S1, S2, A, A], LensFamily[S1, S2, B, B], LensFamily[S1, S2, C, C]) =
    LensFamilyUnzip[S1, S2].unzip3(lens.xmapbB(tuple3B))

  implicit def tuple4LensFamily[S1, S2, A, B, C, D](lens: LensFamily[S1, S2, (A, B, C, D), (A, B, C, D)]):
  (LensFamily[S1, S2, A, A], LensFamily[S1, S2, B, B], LensFamily[S1, S2, C, C], LensFamily[S1, S2, D, D]) =
    LensFamilyUnzip[S1, S2].unzip4(lens.xmapbB(tuple4B))

  implicit def tuple5LensFamily[S1, S2, A, B, C, D, E](lens: LensFamily[S1, S2, (A, B, C, D, E), (A, B, C, D, E)]):
  (LensFamily[S1, S2, A, A], LensFamily[S1, S2, B, B], LensFamily[S1, S2, C, C], LensFamily[S1, S2, D, D], LensFamily[S1, S2, E, E]) =
    LensFamilyUnzip[S1, S2].unzip5(lens.xmapbB(tuple5B))

  implicit def tuple6LensFamily[S1, S2, A, B, C, D, E, H](lens: LensFamily[S1, S2, (A, B, C, D, E, H), (A, B, C, D, E, H)]):
  (LensFamily[S1, S2, A, A], LensFamily[S1, S2, B, B], LensFamily[S1, S2, C, C], LensFamily[S1, S2, D, D], LensFamily[S1, S2, E, E], LensFamily[S1, S2, H, H]) =
    LensFamilyUnzip[S1, S2].unzip6(lens.xmapbB(tuple6B))

  implicit def tuple7LensFamily[S1, S2, A, B, C, D, E, H, I](lens: LensFamily[S1, S2, (A, B, C, D, E, H, I), (A, B, C, D, E, H, I)]):
  (LensFamily[S1, S2, A, A], LensFamily[S1, S2, B, B], LensFamily[S1, S2, C, C], LensFamily[S1, S2, D, D], LensFamily[S1, S2, E, E], LensFamily[S1, S2, H, H], LensFamily[S1, S2, I, I]) =
    LensFamilyUnzip[S1, S2].unzip7(lens.xmapbB(tuple7B))
}

private[scalaz] trait LensCategory
  extends Choice[Lens]
  with Split[Lens] {

  def compose[A, B, C](bc: Lens[B, C], ab: Lens[A, B]): Lens[A, C] = ab >=> bc

  def id[A] = LensFamily.lensId

  def choice[A, B, C](f: => Lens[A, C], g: => Lens[B, C]): Lens[A \/ B, C] =
    LensFamily.lens {
      case -\/(a) =>
        f run a map (\/.left)
      case \/-(b) =>
        g run b map (\/.right)
    }

  def split[A, B, C, D](f: Lens[A, B], g: Lens[C, D]): Lens[(A,  C), (B, D)] =
    f *** g

}
