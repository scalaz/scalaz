package scalaz

import CostateT._
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
sealed trait LensT[F[+_], G[+_], A, B] {
  def run(a: A): F[Costate[B, G[A]]]

  def apply(a: A): F[Costate[B, G[A]]] =
    run(a)

  import StateT._
  import LensT._
  import BijectionT._
  import WriterT._

  def xmapA[X](f: A => X, g: X => A)(implicit FF: Functor[F], GF: Functor[G]): LensT[F, G, X, B] =
    lensT(x => FF.map(run(g(x)))(_ map (GF.map(_)(f))))

  def xmapbA[X](b: Bijection[A, X])(implicit FF: Functor[F], GF: Functor[G]): LensT[F, G, X, B] =
    xmapA(b to _, b from _)

  def xmapB[X](f: B => X, g: X => B)(implicit FF: Functor[F], GF: Functor[G]): LensT[F, G, A, X] =
    lensT(a => FF.map(run(a))(_ xmap (f, g)))

  def xmapbB[X](b: Bijection[B, X])(implicit FF: Functor[F], GF: Functor[G]): LensT[F, G, A, X] =
    xmapB(b to _, b from _)

  def get(a: A)(implicit F: Functor[F]): F[B] =
    F.map(run(a))(_.pos)

  def set(a: A, b: B)(implicit F: Functor[F]): F[G[A]] =
    F.map(run(a))(_.put(b))

  def st(implicit F: Functor[F]): StateT[F, A, B] =
    StateT(s => F.map(get(s))((s, _)))

  /** Modify the value viewed through the lens */
  def mod(f: B => B, a: A)(implicit F: Functor[F], ev: G[A] =:= Id[A]): F[A] =
    F.map(run(a))(c => {
      val (p, q) = c.run
      p(f(q))
    })

  def =>=(f: B => B)(implicit F: Functor[F], ev: G[A] =:= Id[A]): A => F[A] =
    mod(f, _)

  def modf[X[_]](f: B => X[B], a: A)(implicit F: Functor[F], XF: Functor[X], ev: G[A] =:= Id[A]): F[X[A]] =
    F.map(run(a))(c => XF.map(f(c.pos))(c put _))

  def =>>=[X[_]](f: B => X[B])(implicit F: Functor[F], XF: Functor[X], ev: G[A] =:= Id[A]): A => F[X[A]] =
    modf(f, _)

  def %=(f: B => B)(implicit FF: Functor[F], ev: G[A] =:= Id[A]): StateT[F, A, B] =
    StateT(a =>
      FF.map(run(a))(c => {
        val b = f(c.pos)
        (c put b, b)
      }))

  def :=(b: => B)(implicit F: Functor[F], ev: G[A] =:= Id[A]): StateT[F, A, B] =
    %=(_ => b)

  def %==(f: B => B)(implicit F: Functor[F], ev: G[A] =:= Id[A]): StateT[F, A, Unit] =
    StateT(a =>
      F.map(mod(f, a))((_, ())))

  def %%=[C](s: StateT[F, B, C])(implicit M: Bind[F], ev: G[A] =:= Id[A]): StateT[F, A, C] =
    StateT(a => M.bind(run(a))(x =>
      M.map(s(x.pos)){
        case (b, c) => (x put b, c)
      }))

  def >-[C](f: B => C)(implicit F: Functor[F], ev: G[A] =:= Id[A]): StateT[F, A, C] =
    StateT(a => F.map(get(a))(x => (a, f(x))))

  def >>-[C](f: B => StateT[F, A, C])(implicit F: Bind[F], ev: G[A] =:= Id[A]): StateT[F, A, C] =
    StateT(a => F.bind(get(a))(x => f(x)(a)))

  def ->>-[C](f: => StateT[F, A, C])(implicit F: Bind[F], ev: G[A] =:= Id[A]): StateT[F, A, C] =
    >>-(_ => f)

  /** Lenses can be composed */
  def compose[C](that: LensT[F, G, C, A])(implicit FF: Bind[F], GF: Bind[G]): LensT[F, G, C, B] =
    lensT(c =>
      FF.bind(that run c)(x => {
        val (ac, a) = x.run
        FF.map(run(a))(y => {
          val (ba, b) = y.run
          Costate(x => GF.bind(ba(x))(ac), b)
        })
      }))

  /** alias for `compose` */
  def <=<[C](that: LensT[F, G, C, A])(implicit FF: Bind[F], GF: Bind[G]): LensT[F, G, C, B] = compose(that)

  def andThen[C](that: LensT[F, G, B, C])(implicit FF: Bind[F], GF: Bind[G]): LensT[F, G, A, C] =
    that compose this

  /** alias for `andThen` */
  def >=>[C](that: LensT[F, G, B, C])(implicit FF: Bind[F], GF: Bind[G]): LensT[F, G, A, C] = andThen(that)

  /** Two lenses that view a value of the same type can be joined */
  def sum[C](that: => LensT[F, G, C, B])(implicit FF: Functor[F], GF: Functor[G]): LensT[F, G, Either[A, C], B] =
    lensT{
      case Left(a) =>
        FF.map(run(a))(_ map (GF.map(_)(Left(_))))
      case Right(c) =>
        FF.map(that run c)(_ map (GF.map(_)(Right(_))))
    }

  /** Alias for `sum` */
  def |||[C](that: => LensT[F, G, C, B])(implicit FF: Functor[F], GF: Functor[G]): LensT[F, G, Either[A, C], B] = sum(that)

  /** Two disjoint lenses can be paired */
  def product[C, D](that: LensT[F, G, C, D])(implicit FF: Apply[F], GG: Apply[G]): LensT[F, G, (A, C), (B, D)] =
    lensT {
      case (a, c) => FF.map2(run(a), that run c)((x, y) =>
        x *** y map {
          case (q, r) => GG.map2(q, r)((i, j) => (i, j))
        })
    }

  /** alias for `product` */
  def ***[C, D](that: LensT[F, G, C, D])(implicit FF: Apply[F], GG: Apply[G]): LensT[F, G, (A, C), (B, D)] = product(that)

  trait LensLaw {
    def identity(a: A)(implicit A: Equal[A], ev: F[Costate[B, G[A]]] =:= Id[Costate[B, Id[A]]]): Boolean = {
      val c = run(a)
      A.equal(c.put(c.pos), a)
    }
    def retention(a: A, b: B)(implicit B: Equal[B], ev: F[Costate[B, G[A]]] =:= Id[Costate[B, Id[A]]]): Boolean =
      B.equal(run(run(a) put b).pos, b)
    def doubleSet(a: A, b1: B, b2: B)(implicit A: Equal[A], ev: F[Costate[B, G[A]]] =:= Id[Costate[B, Id[A]]]) = {
      val r = run(a)
      A.equal(run(r put b1) put b2, r put b2)
    }
  }

  def lensLaw = new LensLaw {}

  /** A homomorphism of lens categories */
  def partial(implicit F: Functor[F]): PLensT[F, G, A, B] =
    PLensT.plensT(a => F.map(run(a))(x => Some(x):Option[Costate[B,G[A]]]))

  /** alias for `partial` */
  def unary_~(implicit F: Functor[F]) : PLensT[F, G, A, B] =
    partial
}

/*abstract class Lens[A, B] extends LensT[Id, Id, A, B] {
  trait LensLaw {
    def identity(a: A)(implicit A: Equal[A]): Boolean = {
      val c: Costate[B, A] = run(a)
      A.equal(c.put(c.pos), a)
    }
    def retention(a: A, b: B)(implicit B: Equal[B]): Boolean =
      B.equal(run(run(a) put b).pos, b)
    def doubleSet(a: A, b1: B, b2: B)(implicit A: Equal[A]) = {
      val r = run(a)
      A.equal(run(r put b1) put b2, r put b2)
    }
  }

  def lensLaw = new LensLaw {}*/

  /** Modify the value viewed through the lens */
  /*def mod(f: B => B, a: A): A = {
    val c = run(a)
    val (p, q) = c.run
    p(f(q))
  }

  def =>=(f: B => B): A => A =
    mod(f, _)

  def modf[X[+_]](f: B => X[B], a: A)(implicit XF: Functor[X]): X[A] = {
    val c = run(a)
    XF.map(f(c.pos))(c put _)
  }

  def =>>=[X[+_]](f: B => X[B])(implicit XF: Functor[X]): A => X[A] =
    modf(f, _)

  def %=(f: B => B): State[A, B] =
    State(a => {
      val c = run(a)
      val b = f(c.pos)
      (b, c put b)
    })

  def :=(b: => B): State[A, B] =
    %=(_ => b)

  def %==(f: B => B): State[A, Unit] =
    State(a => ((), mod(f, a)))

  def %%=[C](s: State[B, C]): State[A, C] =
    State(a => {
      val x = run(a)
      val (c, b) = s(x.pos)
      (c, x put b)
    })

  def >-[C](f: B => C): State[A, C] =
    State(a => (f(get(a)), a))

  def >>-[C](f: B => State[A, C]): State[A, C] =
    State(a => f(get(a))(a))

  def ->>-[C](f: => State[A, C]): State[A, C] =
    >>-(_ => f)
}*/

object LensT extends LensTFunctions with LensTInstances {
  def apply[F[+_], G[+_], A, B](r: A => F[Costate[B, G[A]]]): LensT[F, G, A, B] =
    lensT(r)
}

trait LensTFunctions {
  import CostateT._

  def lensT[F[+_], G[+_], A, B](r: A => F[Costate[B, G[A]]]): LensT[F, G, A, B] = new LensT[F, G, A, B] {
    def run(a: A): F[Costate[B, G[A]]] = r(a)
  }

  def lens[A, B](r: A => Costate[B, A]): Lens[A, B] =
    lensT[Id, Id, A, B](r)

  def lensp[F[+_], G[+_], A, B](r: A => Costate[B, A])(implicit PF: Pointed[F], PG: Pointed[G]): LensT[F, G, A, B] =
    lensT(a => PF.point(r(a)map (PG.point(_))))

  def lensgT[F[+_], G[+_], A, B](set: A => F[B => G[A]], get: A => F[B])(implicit M: Bind[F]): LensT[F, G, A, B] =
    lensT(a => M.map2(set(a), get(a))(Costate(_, _)))

  def lensg[A, B](set: A => B => A, get: A => B): Lens[A, B] =
    lensgT[Id, Id, A, B](set, get)

  def lensu[A, B](set: (A, B) => A, get: A => B): Lens[A, B] =
    lensg(set.curried, get)

  /** The identity lens for a given object */
  def lensId[F[+_], G[+_], A](implicit PF: Pointed[F], PG: Pointed[G]): LensT[F, G, A, A] =
    lensp(Costate(identity, _))

  /** The trivial lens that can retrieve Unit from anything */
  def trivialLens[F[+_], G[+_], A](implicit PF: Pointed[F], PG: Pointed[G]): LensT[F, G, A, Unit] =
    lensp[F, G, A, Unit](a => Costate(_ => a, ()))

  /** A lens that discards the choice of Right or Left from Either */
  def codiagLens[F[+_]: Pointed, G[+_]: Pointed, A]: LensT[F, G, Either[A, A], A] =
    lensId[F, G, A] ||| lensId[F, G, A]

  /** Access the first field of a tuple */
  def firstLens[A, B]: (A, B) @> A =
    lens {
      case (a, b) => Costate(x => (x, b), a)
    }

  /** Access the second field of a tuple */
  def secondLens[A, B]: (A, B) @> B =
    lens {
      case (a, b) => Costate(x => (a, x), b)
    }

  /** Access the first field of a tuple */
  def lazyFirstLens[A, B]: LazyTuple2[A, B] @> A =
    lens(z => Costate(x => LazyTuple2(x, z._2), z._1))

  /** Access the second field of a tuple */
  def lazySecondLens[A, B]: LazyTuple2[A, B] @> B =
    lens(z => Costate(x => LazyTuple2(z._1, x), z._2))

  def nelHeadLens[A]: NonEmptyList[A] @> A =
    lens(l => Costate(NonEmptyList.nel(_, l.tail), l.head))

  def nelTailLens[A]: NonEmptyList[A] @> List[A] =
    lens(l => Costate(NonEmptyList.nel(l.head, _), l.tail))

  /** Access the value at a particular key of a Map **/
  def mapVLens[K, V](k: K): Map[K, V] @> Option[V] =
    lensg(m => ({
      case None => m - k
      case Some(v) => m.updated(k, v)
    }: Option[V] => Map[K, V]), _ get k)

  def factorLens[A, B, C]: Either[(A, B), (A, C)] @> (A, Either[B, C]) =
    lens(e => Costate({
      case (a, Left(b)) => Left(a, b)
      case (a, Right(c)) => Right(a, c)
    }, e match {
      case Left((a, b)) => (a, Left(b))
      case Right((a, c)) => (a, Right(c))
    }))

  def distributeLens[A, B, C]: (A, Either[B, C]) @> Either[(A, B), (A, C)] =
    lens {
      case (a, e) => Costate({
        case Left((aa, bb)) => (aa, Left(bb))
        case Right((aa, cc)) => (aa, Right(cc))
      }, e match {
        case Left(b) => Left(a, b)
        case Right(c) => Right(a, c)

      })
    }

  def lensSetJoin[F[+_], A, B](lens: LensT[F, F, A, B], b: B, a: A)(implicit F: Bind[F]): F[A] =
    F.bind(lens run a)(_ put b)

  def lensSetLiftF[T[_[_], _], F[+_], A, B](lens: LensT[F, ({type λ[+α] = T[F, α]})#λ, A, B], b: B, a: A)(implicit F: Monad[F], G: Bind[({type λ[+α] = T[F, α]})#λ], T: MonadTrans[T]): T[F, A] =
    G.bind(T liftM (lens run a))(_ put b)

  def lensSetLiftG[T[_[_], _], F[+_], A, B](lens: LensT[({type λ[+α] = T[F, α]})#λ, F, A, B], b: B, a: A)(implicit F: Monad[F], G: Bind[({type λ[+α] = T[F, α]})#λ], T: MonadTrans[T]): T[F, A] =
    G.bind(lens run a)(x => T liftM (x put b: F[A]))

}

trait LensTInstances0 {
  implicit def lensTArrId[F[+_], G[+_]](implicit F0: Pointed[F], G0: Pointed[G]): ArrId[({type λ[α, β] = LensT[F, G, α, β]})#λ] = new LensTArrId[F, G] {
    implicit def F = F0
    implicit def G = G0
  }
}

trait LensTInstances {
  import LensT._
  import collection.immutable.Stack
  import collection.SeqLike
  import collection.immutable.Queue
  import BijectionT._

  implicit def lensTCategory[F[+_], G[+_]](implicit F0: Monad[F], G0: Monad[G]) = new LensTCategory[F, G] {
    implicit def F = F0
    implicit def G = G0
  }

  /** Lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def LensState[F[+_], G[+_], A, B](lens: LensT[F, G, A, B])(implicit FF: Functor[F], GF: Functor[G]): StateT[F, A, B] =
    lens.st

  implicit def LensTUnzip[F[+_], G[+_], S](implicit FF: Functor[F], GF: Functor[G]): Unzip[({type λ[α] = LensT[F, G, S, α]})#λ] =
    new Unzip[({type λ[α] = LensT[F, G, S, α]})#λ] {
      def unzip[A, B](a: LensT[F, G, S, (A, B)]) =
        (
          LensT(x => FF.map(a run x)(c => {
            val (p, q) = c.pos
            Costate(a => c.put((a, q)): G[S], p)
          }))
          , LensT(x => FF.map(a run x)(c => {
          val (p, q) = c.pos
          Costate(a => c.put((p, a)): G[S], q)
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

  implicit def tuple2Lens[F[+_]: Functor, G[+_]: Functor, S, A, B](lens: LensT[F, G, S, (A, B)]):
  (LensT[F, G, S, A], LensT[F, G, S, B]) =
    LensTUnzip[F, G, S].unzip(lens)

  implicit def tuple3Lens[F[+_]: Functor, G[+_]: Functor, S, A, B, C](lens: LensT[F, G, S, (A, B, C)]):
  (LensT[F, G, S, A], LensT[F, G, S, B], LensT[F, G, S, C]) =
    LensTUnzip[F, G, S].unzip3(lens.xmapbB(tuple3B))

  implicit def tuple4Lens[F[+_]: Functor, G[+_]: Functor, S, A, B, C, D](lens: LensT[F, G, S, (A, B, C, D)]):
  (LensT[F, G, S, A], LensT[F, G, S, B], LensT[F, G, S, C], LensT[F, G, S, D]) =
    LensTUnzip[F, G, S].unzip4(lens.xmapbB(tuple4B))

  implicit def tuple5Lens[F[+_]: Functor, G[+_]: Functor, S, A, B, C, D, E](lens: LensT[F, G, S, (A, B, C, D, E)]):
  (LensT[F, G, S, A], LensT[F, G, S, B], LensT[F, G, S, C], LensT[F, G, S, D], LensT[F, G, S, E]) =
    LensTUnzip[F, G, S].unzip5(lens.xmapbB(tuple5B))

  implicit def tuple6Lens[F[+_]: Functor, G[+_]: Functor, S, A, B, C, D, E, H](lens: LensT[F, G, S, (A, B, C, D, E, H)]):
  (LensT[F, G, S, A], LensT[F, G, S, B], LensT[F, G, S, C], LensT[F, G, S, D], LensT[F, G, S, E], LensT[F, G, S, H]) =
    LensTUnzip[F, G, S].unzip6(lens.xmapbB(tuple6B))

  implicit def tuple7Lens[F[+_]: Functor, G[+_]: Functor, S, A, B, C, D, E, H, I](lens: LensT[F, G, S, (A, B, C, D, E, H, I)]):
  (LensT[F, G, S, A], LensT[F, G, S, B], LensT[F, G, S, C], LensT[F, G, S, D], LensT[F, G, S, E], LensT[F, G, S, H], LensT[F, G, S, I]) =
    LensTUnzip[F, G, S].unzip7(lens.xmapbB(tuple7B))
}

private[scalaz] trait LensTArrId[F[+_], G[+_]]
  extends ArrId[({type λ[α, β] = LensT[F, G, α, β]})#λ]{

  implicit def F: Pointed[F]
  implicit def G: Pointed[G]

  def id[A] = LensT.lensId
}

private[scalaz] trait LensTCategory[F[+_], G[+_]]
  extends Choice[({type λ[α, β] = LensT[F, G, α, β]})#λ]
  with Split[({type λ[α, β] = LensT[F, G, α, β]})#λ]
  with LensTArrId[F, G] {

  implicit def F: Monad[F]
  implicit def G: Monad[G]

  def compose[A, B, C](bc: LensT[F, G, B, C], ab: LensT[F, G, A, B]): LensT[F, G, A, C] = ab >=> bc

  def choice[A, B, C](f: => LensT[F, G, A, C], g: => LensT[F, G, B, C]): LensT[F, G, Either[A, B], C] =
    LensT.lensT {
      case Left(a) =>
        F.map(f run a)(_ map (G.map(_)(Left(_))))
      case Right(b) =>
        F.map(g run b)(_ map (G.map(_)(Right(_))))
    }

  def split[A, B, C, D](f: LensT[F, G, A, B], g: LensT[F, G, C, D]): LensT[F, G, (A,  C), (B, D)] =
    f *** g

}
