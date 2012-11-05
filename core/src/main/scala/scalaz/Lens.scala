package scalaz

import IndexedStoreT._, StoreT._
import Id._

/**
 * A Lens family, offering a purely functional means to access and retrieve
 * a field transitioning from type `B1` to type `B21 in a record simultaneously
 * transitioning from type `A1` to type `A2`.  [[scalaz.Lens]] is a convenient
 * alias for when `A1 =:= A2` and `B1 =:= B2`.
 *
 * The term ''field'' should not be interpreted restrictively to mean a member of a class. For example, a lens
 * can address membership of a `Set`.
 *
 * @see [[scalaz.PLens]]
 *
 * @tparam A1 The initial type of the record
 * @tparam A2 The final type of the record
 * @tparam B1 The initial type of the field
 * @tparam B2 The final type of the field
 */
sealed trait LensFamilyT[F[+_], -A1, +A2, +B1, -B2] {
  def run(a: A1): F[IndexedStore[B1, B2, A2]]

  def apply(a: A1): F[IndexedStore[B1, B2, A2]] =
    run(a)

  import StateT._
  import LensFamilyT._
  import BijectionT._
  import WriterT._

  def mapC[C1, C2](f: IndexedStore[B1, B2, A2] => IndexedStore[C1, C2, A2])(implicit F: Functor[F]): PLensFamilyT[F, A1, A2, C1, C2] =
    lensFamilyT(a => F.map(run(a))(f))

  def xmapA[X1, X2](f: A2 => X2)(g: X1 => A1)(implicit F: Functor[F]): LensFamilyT[F, X1, X2, B1, B2] =
    lensFamilyT(x => F.map(run(g(x)))(_ map (f)))

  def xmapbA[X, A >: A2 <: A1](b: Bijection[A, X])(implicit F: Functor[F]): LensFamilyT[F, X, X, B1, B2] =
    xmapA(b to _)(b from _)

  def xmapB[X1, X2](f: B1 => X1)(g: X2 => B2)(implicit F: Functor[F]): LensFamilyT[F, A1, A2, X1, X2] =
    lensFamilyT(a => F.map(run(a))(_.xmap(f)(g)))

  def xmapbB[X, B >: B1 <: B2](b: Bijection[B, X])(implicit F: Functor[F]): LensFamilyT[F, A1, A2, X, X] =
    xmapB(b to _)(b from _)

  def get(a: A1)(implicit F: Functor[F]): F[B1] =
    F.map(run(a))(_.pos)

  def set(a: A1, b: B2)(implicit F: Functor[F]): F[A2] =
    F.map(run(a))(_.put(b))

  def st(implicit F: Functor[F]): StateT[F, A1, B1] =
    StateT(s => F.map(get(s))((s, _)))

  /** Modify the value viewed through the lens */
  def mod(f: B1 => B2, a: A1)(implicit F: Functor[F]): F[A2] =
    F.map(run(a))(c => {
      val (p, q) = c.run
      p(f(q))
    })

  def =>=(f: B1 => B2)(implicit F: Functor[F]): A1 => F[A2] =
    mod(f, _)

  /** Modify the value viewed through the lens, returning a functor `X` full of results. */
  def modf[X[_]](f: B1 => X[B2], a: A1)(implicit F: Functor[F], XF: Functor[X]): F[X[A2]] =
    F.map(run(a))(c => XF.map(f(c.pos))(c put _))

  def =>>=[X[_]](f: B1 => X[B2])(implicit F: Functor[F], XF: Functor[X]): A1 => F[X[A2]] =
    modf(f, _)

  /** Modify the value viewed through the lens, returning a `C` on the side.  */
  def modp[C](f: B1 => F[(B2, C)], a: A1)(implicit F: Bind[F]): F[(A2, C)] = F.bind(
    get(a))(x => F.bind(
    f(x)){
      case (b, c) => F.map(set(a, b))((_, c))
    })

  /** Modify the portion of the state viewed through the lens and return its new value. */
  def mods[A >: A2 <: A1, B >: B1 <: B2](f: B1 => B)(implicit F: Functor[F]): StateT[F, A, B] =
    StateT(a =>
      F.map(run(a))(c => {
        val b = f(c.pos)
        (c put b, b)
      }))

  /** Modify the portion of the state viewed through the lens and return its new value. */
  def %=[A >: A2 <: A1, B >: B1 <: B2](f: B1 => B)(implicit F: Functor[F]): StateT[F, A, B] =
    mods[A, B](f)

  /** Set the portion of the state viewed through the lens and return its new value. */
  def assign[A >: A2 <: A1, B >: B1 <: B2](b: => B)(implicit F: Functor[F]): StateT[F, A, B] =
    mods[A, B](_ => b)

  /** Set the portion of the state viewed through the lens and return its new value. */
  def :=[A >: A2 <: A1, B >: B1 <: B2](b: => B)(implicit F: Functor[F]): StateT[F, A, B] =
    assign[A, B](b)

  /** Modify the portion of the state viewed through the lens, but do not return its new value. */
  def mods_[A >: A2 <: A1](f: B1 => B2)(implicit F: Functor[F]): StateT[F, A, Unit] =
    StateT(a =>
      F.map(mod(f, a))((_, ())))

  /** Modify the portion of the state viewed through the lens, but do not return its new value. */
  def %==[A >: A2 <: A1](f: B1 => B2)(implicit F: Functor[F]): StateT[F, A, Unit] =
    mods_(f)

  /** Contravariantly map a state action through a lens. */
  def lifts[C, A >: A2 <: A1, B >: B1 <: B2](s: StateT[F, B, C])(implicit M: Bind[F]): StateT[F, A, C] =
    StateT(a => modp(s(_), a))

  def %%=[C, A >: A2 <: A1, B >: B1 <: B2](s: StateT[F, B, C])(implicit M: Bind[F]): StateT[F, A, C] =
    lifts(s)

  /** Map the function `f` over the lens as a state action. */
  def map[C, A >: A2 <: A1](f: B1 => C)(implicit F: Functor[F]): StateT[F, A, C] =
    StateT(a => F.map(get(a))(x => (a, f(x))))

  /** Map the function `f` over the value under the lens, as a state action. */
  def >-[C, A >: A2 <: A1](f: B1 => C)(implicit F: Functor[F]): StateT[F, A, C] = map(f)

  /** Bind the function `f` over the value under the lens, as a state action. */
  def flatMap[C, A >: A2 <: A1, B >: B1 <: B2](f: B1 => StateT[F, A, C])(implicit F: Bind[F]): StateT[F, A, C] =
    StateT(a => F.bind(get(a))(x => f(x)(a)))

  /** Bind the function `f` over the value under the lens, as a state action. */
  def >>-[C, A >: A2 <: A1, B >: B1 <: B2](f: B1 => StateT[F, A, C])(implicit F: Bind[F]): StateT[F, A, C] = flatMap[C, A, B](f)

  /** Sequence the monadic action of looking through the lens to occur before the state action `f`. */
  def ->>-[C, A >: A2 <: A1](f: => StateT[F, A, C])(implicit F: Bind[F], ev: B1 <:< B2): StateT[F, A, C] =
    mapB1(ev).>>-(_ => f)

  /** Contravariantly mapping the state of a state monad through a lens is a natural transformation */
  def liftsNT[A >: A2 <: A1, B >: B1 <: B2](implicit F: Bind[F]): ({type m[x] = StateT[F,B,x]})#m ~> ({type n[x] = StateT[F,A,x]})#n =
    new (({type m[x] = StateT[F,B,x]})#m ~> ({type n[x] = StateT[F,A,x]})#n) {
      def apply[C](s : StateT[F,B,C]): StateT[F,A,C] = StateT[F,A,C](a => modp(s(_), a))
    }

  /** Lenses can be composed */
  def compose[C1, C2](that: LensFamilyT[F, C1, C2, A1, A2])(implicit F: Bind[F]): LensFamilyT[F, C1, C2, B1, B2] =
    lensFamilyT(c =>
      F.bind(that run c)(x => {
        val (ac, a) = x.run
        F.map(run(a))(y => {
          val (ba, b) = y.run
          IndexedStore(ac compose ba, b)
        })
      }))

  /** alias for `compose` */
  def <=<[C1, C2](that: LensFamilyT[F, C1, C2, A1, A2])(implicit F: Bind[F]): LensFamilyT[F, C1, C2, B1, B2] = compose(that)

  def andThen[C1, C2](that: LensFamilyT[F, B1, B2, C1, C2])(implicit F: Bind[F]): LensFamilyT[F, A1, A2, C1, C2] =
    that compose this

  /** alias for `andThen` */
  def >=>[C1, C2](that: LensFamilyT[F, B1, B2, C1, C2])(implicit F: Bind[F]): LensFamilyT[F, A1, A2, C1, C2] = andThen(that)

  /** Two lenses that view a value of the same type can be joined */
  def sum[C1, C2](that: => LensFamilyT[F, C1, C2, B1, B2])(implicit F: Functor[F]): LensFamilyT[F, A1 \/ C1, A2 \/ C2, B1, B2] =
    lensFamilyT{
      case -\/(a) =>
        F.map(run(a))(_ map (-\/(_)))
      case \/-(c) =>
        F.map(that run c)(_ map (\/-(_)))
    }

  /** Alias for `sum` */
  def |||[C1, C2](that: => LensFamilyT[F, C1, C2, B1, B2])(implicit F: Functor[F]): LensFamilyT[F, A1 \/ C1, A2 \/ C2, B1, B2] = sum(that)

  /** Two disjoint lenses can be paired */
  def product[C1, C2, D1, D2](that: LensFamilyT[F, C1, C2, D1, D2])(implicit F: Apply[F]): LensFamilyT[F, (A1, C1), (A2, C2), (B1, D1), (B2, D2)] =
    lensFamilyT {
      case (a, c) => F.apply2(run(a), that run c)((x, y) => x *** y)
    }

  /** alias for `product` */
  def ***[C1, C2, D1, D2](that: LensFamilyT[F, C1, C2, D1, D2])(implicit F: Apply[F]): LensFamilyT[F, (A1, C1), (A2, C2), (B1, D1), (B2, D2)] = product(that)

  trait LensLaw {
    def identity[A >: A2 <: A1, B >: B1 <: B2](a: A)(implicit A: Equal[A], ev: F[Store[B, A]] =:= Id[Store[B, A]]): Boolean = {
      val c = run(a)
      A.equal(c.put(c.pos), a)
    }
    def retention[A >: A2 <: A1, B >: B1 <: B2](a: A, b: B)(implicit B: Equal[B], ev: F[Store[B, A]] =:= Id[Store[B, A]]): Boolean =
      B.equal(run(run(a) put b).pos, b)
    def doubleSet[A >: A2 <: A1, B >: B1 <: B2](a: A, b1: B, b2: B)(implicit A: Equal[A], ev: F[Store[B, A]] =:= Id[Store[B, A]]): Boolean = {
      val r = run(a)
      A.equal(run(r put b1) put b2, r put b2)
    }
  }

  def lensLaw = new LensLaw {}

  /** A homomorphism of lens categories */
  def partial(implicit F: Functor[F]): PLensFamilyT[F, A1, A2, B1, B2] =
    PLensFamilyT.plensFamilyT(a => F.map(run(a))(x => Some(x):Option[IndexedStore[B1, B2, A2]]))

  /** alias for `partial` */
  def unary_~(implicit F: Functor[F]): PLensFamilyT[F, A1, A2, B1, B2] =
    partial
}

object LensFamilyT extends LensTFunctions with LensTInstances {
  def apply[F[+_], A1, A2, B1, B2](r: A1 => F[IndexedStore[B1, B2, A2]]): LensFamilyT[F, A1, A2, B1, B2] =
    lensFamilyT(r)
}

trait LensFamilyTFunctions {
  import IndexedStoreT._, StoreT._

  def lensFamilyT[F[+_], A1, A2, B1, B2](r: A1 => F[IndexedStore[B1, B2, A2]]): LensFamilyT[F, A1, A2, B1, B2] = new LensFamilyT[F, A1, A2, B1, B2] {
    def run(a: A1): F[IndexedStore[B1, B2, A2]] = r(a)
  }

  def lensFamily[A1, A2, B1, B2](r: A1 => IndexedStore[B1, B2, A2]): LensFamily[A1, A2, B1, B2] =
    lensFamilyT[Id, A1, A2, B1, B2](r)

  def lensFamilyp[F[+_], A1, A2, B1, B2](r: A1 => IndexedStore[B1, B2, A2])(implicit F: Pointed[F]): LensFamilyT[F, A1, A2, B1, B2] =
    lensFamilyT(a => F.point(r(a)))

  def lensFamilygT[F[+_], A1, A2, B1, B2](set: A1 => F[B2 => A2], get: A1 => F[B1])(implicit M: Bind[F]): LensFamilyT[F, A1, A2, B1, B2] =
    lensFamilyT(a => M.apply2(set(a), get(a))(IndexedStore(_, _)))

  def lensFamilyg[A1, A2, B1, B2](set: A1 => B2 => A2, get: A1 => B1): LensFamily[A1, A2, B1, B2] =
    lensFamilygT[Id, A1, A2, B1, B2](set, get)

  def lensFamilyu[A1, A2, B1, B2](set: (A1, B2) => A2, get: A1 => B1): LensFamily[A1, A2, B1, B2] =
    lensFamilyg(set.curried, get)

  /** The identity lens family for a given pair of objects */
  def lensFamilyId[F[+_], A1, A2](implicit P: Pointed[F]): LensFamilyT[F, A1, A2, A1, A2] =
    lensFamilyp(IndexedStore(identity, _))

  /** A lens family that discards the choice of right or left from disjunction */
  def codiagLensFamily[F[+_]: Pointed, A1, A2]: LensFamilyT[F, A1 \/ A1, A2 \/ A2, A1, A2] =
    lensFamilyId[F, A1, A2] ||| lensFamilyId[F, A1, A2]

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
    LensFamily(q => IndexedStore(_ match {
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

  def distributeLens[A1, A2, B1, B2, C1, C2]: LensFamily[(A1, B1 \/ C1), (A2, B2 \/ C2), ((A1, B1) \/ (A1, C1)), ((A2, B2) \/ (A2, C2))] =
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

trait LensTFunctions extends LensFamilyTFunctions {
  import IndexedStoreT._, StoreT._

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

trait LensTInstances extends LensTInstances0 {
  import LensFamilyT._
  import collection.immutable.Stack
  import collection.SeqLike
  import collection.immutable.Queue
  import BijectionT._

  implicit def lensTCategory[F[+_]](implicit F0: Monad[F]) = new LensTCategory[F] {
    implicit def F = F0
  }

  /** Lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def LensFamilyState[F[+_], A, B](lens: LensFamilyT[F, A, _, B, _])(implicit F: Functor[F]): StateT[F, A, B] =
    lens.st

  implicit def LensFamilyTUnzip[F[+_], S, R](implicit F: Functor[F]): Unzip[({type λ[α] = LensFamilyT[F, S, R, α, α]})#λ] =
    new Unzip[({type λ[α] = LensFamilyT[F, S, R, α, α]})#λ] {
      def unzip[A, B](a: LensFamilyT[F, S, R, (A, B), (A, B)]) =
        (
          LensFamilyT(x => F.map(a run x)(c => {
            val (p, q) = c.pos
            IndexedStore(a => c.put((a, q)): R, p)
          }))
          , LensFamilyT(x => F.map(a run x)(c => {
          val (p, q) = c.pos
          IndexedStore(a => c.put((p, a)): R, q)
        }))
        )
    }

  type SetLens[S, K] = SetLens[S, S, K]
  val SetLens: SetLensFamily.type = SetLensFamily
  case class SetLensFamily[-S1, +S2, K](lens: LensFamily[S1, S2, Set[K], Set[K]]) {
    /** Setting the value of this lens will change whether or not it is present in the set */
    def contains(key: K) = LensFamilyT.lensFamilyg[S1, S2, Boolean, Boolean](
      s => b => lens.mod(m => if (b) m + key else m - key, s): Id[S2]
    , s => lens.get(s).contains(key)
    )

    def &=[S >: S2 <: S1](that: Set[K]): State[S, Set[K]] =
      lens %= (_ & that)

    def &~=[S >: S2 <: S1](that: Set[K]): State[S, Set[K]] =
      lens %= (_ &~ that)

    def |=[S >: S2 <: S1](that: Set[K]): State[S, Set[K]] =
      lens %= (_ | that)

    def +=[S >: S2 <: S1](elem: K): State[S, Set[K]] =
      lens %= (_ + elem)

    def +=[S >: S2 <: S1](elem1: K, elem2: K, elems: K*): State[S, Set[K]] =
      lens %= (_ + elem1 + elem2 ++ elems)

    def ++=[S >: S2 <: S1](xs: TraversableOnce[K]): State[S, Set[K]] =
      lens %= (_ ++ xs)

    def -=[S >: S2 <: S1](elem: K): State[S, Set[K]] =
      lens %= (_ - elem)

    def -=[S >: S2 <: S1](elem1: K, elem2: K, elems: K*): State[S, Set[K]] =
      lens %= (_ - elem1 - elem2 -- elems)

    def --=[S >: S2 <: S1](xs: TraversableOnce[K]): State[S, Set[K]] =
      lens %= (_ -- xs)
  }

  /** A lens that views a Set can provide the appearance of in place mutation */
  implicit def setLensFamily[S1, S2, K](lens: LensFamily[S1, S2, Set[K], Set[K]]) =
    SetLensFamily[S1, S2, K](lens)

  type MapLens[S, K, V] = MapLensFamily[S, S, K, V]
  val MapLens: MapLensFamily.type = MapLensFamily
  /** A lens that views an immutable Map type can provide a mutable.Map-like API via State */
  case class MapLensFamily[-S1, +S2, K, V](lens: LensFamily[S1, S2, Map[K, V], Map[K, V]]) {
    /** Allows both viewing and setting the value of a member of the map */
    def member(k: K): LensFamily[S1, S2, Option[V], Option[V]] = lensFamilyg[S1, S2, Option[V], Option[V]](
      s => opt => lens.mod((m: Map[K, V]) => (opt match {
        case Some(v) => m + (k -> v)
        case None    => m - k
      }): Map[K, V], s): Id[S]
      , s => lens.get(s).get(k))

    /** This lens has undefined behavior when accessing an element not present in the map! */
    def at(k: K): LensFamily[S1, S2, V, V] =
      lensFamilyg[S, V](s => v => lens.mod(_ + (k -> v), s): Id[S], lens.get(_) apply k)

    def +=[S >: S2 <: S1](elem1: (K, V), elem2: (K, V), elems: (K, V)*): State[S, Map[K, V]] =
      lens %= (_ + elem1 + elem2 ++ elems)

    def +=[S >: S2 <: S1](elem: (K, V)): State[S, Map[K, V]] =
      lens %= (_ + elem)

    def ++=[S >: S2 <: S1](xs: TraversableOnce[(K, V)]): State[S, Map[K, V]] =
      lens %= (_ ++ xs)

    def update[S >: S2 <: S1](key: K, value: V): State[S, Unit] =
      lens %== (_.updated(key, value))

    def -=[S >: S2 <: S1](elem: K): State[S, Map[K, V]] =
      lens %= (_ - elem)

    def -=[S >: S2 <: S1](elem1: K, elem2: K, elems: K*): State[S, Map[K, V]] =
      lens %= (_ - elem1 - elem2 -- elems)

    def --=[S >: S2 <: S1](xs: TraversableOnce[K]): State[S, Map[K, V]] =
      lens %= (_ -- xs)
  }

  implicit def mapLensFamily[S1, S2, K, V](lens: LensFamily[S1, S2, Map[K, V], Map[K, v]]) =
    MapLensFamily[S1, S2, K, V](lens)

  /** Provide the appearance of a mutable-like API for sorting sequences through a lens */
  case class SeqLikeLens[S, A, Repr <: SeqLike[A, Repr]](lens: Lens[S, Repr]) {
    def sortWith(lt: (A, A) => Boolean): State[S, Unit] =
      lens %== (_ sortWith lt)

    def sortBy[B: math.Ordering](f: A => B): State[S, Unit] =
      lens %== (_ sortBy f)

    def sort[B >: A](implicit ord: math.Ordering[B]): State[S, Unit] =
      lens %== (_.sorted[B])
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

  type ArrayLens[S, A] = ArrayLensFamily[S, S, A]
  val ArrayLens: ArrayLensFamily.type = ArrayLensFamily
  /** Provide an imperative-seeming API for arrays viewed through a lens */
  case class ArrayLensFamily[-S1, +S2, A](lens: LensFamily[S1, S2, Array[A], Array[A]]) {
    def at(n: Int): (LensFamily[S1, S2, A, A]) =
      lensFamilyg[S1, S2, A, A](
        s => v => lens.mod(array => {
          val copy = array.clone()
          copy.update(n, v)
          copy
        }, s): Id[S2]
        , s => lens.get(s) apply n
      )

    def length[S >: S2 <: S1]: State[S, Int] =
      lens >- (_.length)
  }

  implicit def arrayLensFamily[S1, S2, A](lens: LensFamily[S1, S3, Array[A], Array[A]]) =
    ArrayLensFamily[S1, S2, A](lens)

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

  implicit def tuple2LensFamily[F[+_]: Functor, S1, S2, A, B](lens: LensFamilyT[F, S1, S2, (A, B), (A, B)]):
  (LensFamilyT[F, S1, S2, A, A], LensFamilyT[F, S1, S2, B, B]) =
    LensFamilyTUnzip[F, S1, S2].unzip(lens)

  implicit def tuple3LensFamily[F[+_]: Functor, S1, S2, A, B, C](lens: LensFamilyT[F, S1, S2, (A, B, C), (A, B, C)]):
  (LensFamilyT[F, S1, S2, A, A], LensFamilyT[F, S1, S2, B, B], LensFamilyT[F, S1, S2, C, C]) =
    LensFamilyTUnzip[F, S1, S2].unzip3(lens.xmapbB(tuple3B))

  implicit def tuple4LensFamily[F[+_]: Functor, S1, S2, A, B, C, D](lens: LensFamilyT[F, S1, S2, (A, B, C, D), (A, B, C, D)]):
  (LensFamilyT[F, S1, S2, A, A], LensFamilyT[F, S1, S2, B, B], LensFamilyT[F, S1, S2, C, C], LensFamilyT[F, S1, S2, D, D]) =
    LensFamilyTUnzip[F, S1, S2].unzip4(lens.xmapbB(tuple4B))

  implicit def tuple5LensFamily[F[+_]: Functor, S1, S2, A, B, C, D, E](lens: LensFamilyT[F, S1, S2, (A, B, C, D, E), (A, B, C, D, E)]):
  (LensFamilyT[F, S1, S2, A, A], LensFamilyT[F, S1, S2, B, B], LensFamilyT[F, S1, S2, C, C], LensFamilyT[F, S1, S2, D, D], LensFamilyT[F, S1, S2, E, E]) =
    LensFamilyTUnzip[F, S1, S2].unzip5(lens.xmapbB(tuple5B))

  implicit def tuple6LensFamily[F[+_]: Functor, S1, S2, A, B, C, D, E, H](lens: LensFamilyT[F, S1, S2, (A, B, C, D, E, H), (A, B, C, D, E, H)]):
  (LensFamilyT[F, S1, S2, A, A], LensFamilyT[F, S1, S2, B, B], LensFamilyT[F, S1, S2, C, C], LensFamilyT[F, S1, S2, D, D], LensFamilyT[F, S1, S2, E, E], LensFamilyT[F, S1, S2, H, H]) =
    LensFamilyTUnzip[F, S1, S2].unzip6(lens.xmapbB(tuple6B))

  implicit def tuple7LensFamily[F[+_]: Functor, S1, S2, A, B, C, D, E, H, I](lens: LensFamilyT[F, S1, S2, (A, B, C, D, E, H, I), (A, B, C, D, E, H, I)]):
  (LensFamilyT[F, S1, S2, A, A], LensFamilyT[F, S1, S2, B, B], LensFamilyT[F, S1, S2, C, C], LensFamilyT[F, S1, S2, D, D], LensFamilyT[F, S1, S2, E, E], LensFamilyT[F, S1, S2, H, H], LensFamilyT[F, S1, S2, I, I]) =
    LensFamilyTUnzip[F, S1, S2].unzip7(lens.xmapbB(tuple7B))
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
