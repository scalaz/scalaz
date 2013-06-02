package scalaz

/**
 * Partial Lens Families, offering a purely functional means to access and retrieve
 * an optional field transitioning from type `B1` to type `B2` in a record that is
 * simultaneously transitioning from type `A1` to type `A2`.  [[scalaz.PLens]] is a
 * convenient alias for when `A1 =:= A2`, and `B1 =:= B2`.
 *
 * The term ''field'' should not be interpreted restrictively to mean a member of a class. For example, a partial lens
 * family can address the nth element of a `List`.
 *
 * @see [[scalaz.Lens]]
 *
 * @tparam A1 The initial type of the record
 * @tparam A2 The final type of the record
 * @tparam B1 The initial type of the optional field
 * @tparam B2 The final type of the optional field
 */
sealed trait PLensFamily[-A1, +A2, +B1, -B2] {
  def run(a: A1): Option[IndexedStore[B1, B2, A2]]

  def apply(a: A1): Option[IndexedStore[B1, B2, A2]] =
    run(a)

  /*

  import StateT._


   */
  import BijectionT._
  import PLensFamily._

  def kleisli: Kleisli[Option, A1, IndexedStore[B1, B2, A2]] =
    Kleisli[Option, A1, IndexedStore[B1, B2, A2]](apply(_))

  def xmapA[X1, X2](f: A2 => X2)(g: X1 => A1): PLensFamily[X1, X2, B1, B2] =
    plensFamily(x => apply(g(x)) map (_ map (f)))

  def xmapbA[X, A >: A2 <: A1](b: Bijection[A, X]): PLensFamily[X, X, B1, B2] =
    xmapA(b to _)(b from _)

  def xmapB[X1, X2](f: B1 => X1)(g: X2 => B2): PLensFamily[A1, A2, X1, X2] =
    plensFamily(a => apply(a) map (_.xmap(f)(g)))

  def xmapbB[X, B >: B1 <: B2](b: Bijection[B, X]): PLensFamily[A1, A2, X, X] =
    xmapB(b to _)(b from _)

  def get(a: A1): Option[B1] =
    run(a) map (_.pos)

  def getK: Kleisli[Option, A1, B1] =
    Kleisli[Option, A1, B1](get(_))

  /** If the Partial Lens is null, then return the given default value. */
  def getOr[B >: B1](a: A1, b: => B): B =
    get(a) getOrElse b

  def getOrZ[B >: B1](a: A1)(implicit M: Monoid[B]): B =
    getOr(a, M.zero)

  def set(a: A1, b: B2): Option[A2] =
    run(a) map (_.put(b))

  def setK(a: A1): Kleisli[Option, B2, A2] =
    Kleisli[Option, B2, A2](set(a, _))

  def setOr[A >: A2](a: A1, b: B2, d: => A): A =
    set(a, b) getOrElse d

  def setOrZ[A >: A2](a: A1, b: B2)(implicit M: Monoid[A]): A =
    setOr(a, b, M.zero)

  def trySet(a: A1): Option[B2 => A2] =
    run(a) map (c => c put _)

  def trySetK: Kleisli[Option, A1, B2 => A2] =
    Kleisli[Option, A1, B2 => A2](trySet(_))

  def trySetOr[A >: A2, B <: B2](a: A1, d: => B => A): B => A =
    trySet(a) getOrElse d

  def trySetOrZ[A >: A2, B <: B2](a: A1)(implicit M: Monoid[B => A]): B => A =
    trySetOr(a, M.zero)

  /** If the Partial Lens is null, then return the target object, otherwise run the function on its projection. */
  def as[A <: A1](f: B1 => A, a: A): A =
    get(a) match {
      case None => a
      case Some(w) => f(w)
    }

  def is(a: A1): Boolean =
    run(a).isDefined

  def isNot(a: A1): Boolean =
    !is(a)

  def exists(p: B1 => Boolean, a: A1): Boolean =
    get(a) exists p

  def forall(p: B1 => Boolean, a: A1): Boolean =
    get(a) forall p

  def modg(f: B1 => B2, a: A1): Option[A2] =
    run(a).map(_ puts f)

  def =?>=(f: B1 => B2): A1 => Option[A2] =
    modg(f, _)

  /** Modify the potential value viewed through the partial lens */
  def mod[A >: A2 <: A1](f: B1 => B2, a: A): A =
    run(a) match {
      case None => a: A
      case Some(w) => (w puts f): A
    }

  def =>=[A >: A2 <: A1](f: B1 => B2): A => A =
    mod(f, _)

  def st[A <: A1]: PState[A, B1] =
    State(s => (s, get(s)))

  def %=[A >: A2 <: A1, B <: B2](f: B1 => B): PState[A, B] =
    State(a => run(a) match {
      case None => (a, None)
      case Some(w) => {
        val r = f(w.pos)
        (w put r, Some(r))
      }
    })

  def :=[A >: A2 <: A1, B <: B2](b: => B): PState[A, B] =
    %=(_ => b)

  def %==[A >: A2 <: A1, B <: B2](f: B1 => B): State[A, Unit] =
    State(a =>
  (mod(f, a), ()))

  def %%=[A >: A2 <: A1, C](s: IndexedState[B1, B2, C]): PState[A, C] =
    State(a => run(a) match {
      case None => (a, None)
      case Some(w) => {
        val r = s.run(w.pos): (B2, C)
        (w put r._1, Some(r._2))
      }
    })

  def >-[A >: A2 <: A1, C](f: B1 => C): PState[A, C] =
    State(a => (a, get(a) map f))

  def >>-[A >: A2 <: A1, C](f: B1 => State[A, C]): PState[A, C] =
    StateT(a => get(a) match {
      case None => (a, None)
      case Some(w) =>
        f(w) apply a match {
          case (y, x) => (y, Some(x))
        }
    })

  def ->>-[A >: A2 <: A1, C](f: => State[A, C]): PState[A, C] =
    >>-(_ => f)

  /** Partial Lenses can be composed */
  def compose[C1, C2](that: PLensFamily[C1, C2, A1, A2]): PLensFamily[C1, C2, B1, B2] =
    plensFamily(c =>
      (that run c).flatMap (x => {
        val (ac, a) = x.run
        run(a) map (y => {
          val (ba, b) = y.run
          IndexedStore(ac compose ba, b)
        })
      }))

  /** alias for `compose` */
  def <=<[C1, C2](that: PLensFamily[C1, C2, A1, A2]): PLensFamily[C1, C2, B1, B2] = compose(that)

  def andThen[C1, C2](that: PLensFamily[B1, B2, C1, C2]): PLensFamily[A1, A2, C1, C2] =
    that compose this

  /** alias for `andThen` */
  def >=>[C1, C2](that: PLensFamily[B1, B2, C1, C2]): PLensFamily[A1, A2, C1, C2] = andThen(that)

  /** Two partial lenses that view a value of the same type can be joined */
  def sum[C1, C2, B1m >: B1, B2m <: B2](that: => PLensFamily[C1, C2, B1m, B2m]): PLensFamily[A1 \/ C1, A2 \/ C2, B1m, B2m] =
    plensFamily {
      case -\/(a) =>
        run(a) map (_ map (-\/(_)))
      case \/-(c) =>
        that run c map (_ map (\/-(_)))
    }

  /** Alias for `sum` */
  def |||[C1, C2, B1m >: B1, B2m <: B2](that: => PLensFamily[C1, C2, B1m, B2m]): PLensFamily[A1 \/ C1, A2 \/ C2, B1m, B2m] = sum(that)

  /** Two disjoint partial lenses can be paired */
  def product[C1, C2, D1, D2](that: PLensFamily[C1, C2, D1, D2]): PLensFamily[(A1, C1), (A2, C2), (B1, D1), (B2, D2)] =
    plensFamily {
      case (a, c) =>
        for {
          q <- run(a)
          r <- that run c
        } yield q *** r
    }

  /** alias for `product` */
  def ***[C1, C2, D1, D2](that: PLensFamily[C1, C2, D1, D2]): PLensFamily[(A1, C1), (A2, C2), (B1, D1), (B2, D2)] = product(that)

}

object PLensFamily extends PLensFunctions with PLensInstances {

  def apply[A1, A2, B1, B2](r: A1 => Option[IndexedStore[B1, B2, A2]]): PLensFamily[A1, A2, B1, B2] =
    plensFamily(r)
}

trait PLensFamilyFunctions extends PLensInstances {

  import BijectionT._

  def plensFamily[A1, A2, B1, B2](r: A1 => Option[IndexedStore[B1, B2, A2]]): PLensFamily[A1, A2, B1, B2] = new PLensFamily[A1, A2, B1, B2] {
    def run(a: A1): Option[IndexedStore[B1, B2, A2]] = r(a)
  }

  def plensFamilyf[A1, A2, B1, B2](r: PartialFunction[A1, IndexedStore[B1, B2, A2]]): PLensFamily[A1, A2, B1, B2] =
    plensFamily(r.lift)

  def plensFamilyg[A1, A2, B1, B2](set: A1 => Option[B2 => A2], get: A1 => Option[B1]): PLensFamily[A1, A2, B1, B2] =
    plensFamily(a => for {
      w <- set(a)
      x <- get(a)
    } yield IndexedStore(w, x))

  /** The identity partial lens family for a given pair of objects */
  def plensFamilyId[A1, A2]: PLensFamily[A1, A2, A1, A2] =
    LensFamily.lensFamilyId[A1, A2].partial

  /** A partial lens family that discards the choice of right or left from disjunction */
  def codiagPLensFamily[A1, A2]: PLensFamily[A1 \/ A1, A2 \/ A2, A1, A2] =
    plensFamilyId[A1, A2] ||| plensFamilyId[A1, A2]

  /** The always-null partial lens family */
  def nilFamily[A1, A2, B1, B2]: PLensFamily[A1, A2, B1, B2] =
    plensFamily(_ => None)

  def somePLensFamily[A1, A2]: PLensFamily[Option[A1], Option[A2], A1, A2] =
    plensFamily(_ map (z => IndexedStore(Some(_), z)))

  def leftPLensFamily[A1, A2, B]: PLensFamily[A1 \/ B, A2 \/ B, A1, A2] =
    plensFamily {
      case -\/(a) => Some(IndexedStore(-\/(_), a))
      case \/-(_) => None
    }

  def rightPLensFamily[A, B1, B2]: PLensFamily[A \/ B1, A \/ B2, B1, B2] =
    plensFamily {
      case \/-(b) => Some(IndexedStore(\/-(_), b))
      case -\/(_) => None
    }

  def tuple2PLensFamily[S1, S2, A, B](lens: PLensFamily[S1, S2, (A, B), (A, B)]):
  (PLensFamily[S1, S2, A, A], PLensFamily[S1, S2, B, B]) =
    PLensFamilyUnzip[S1, S2].unzip(lens)

  def tuple3PLensFamily[S1, S2, A, B, C](lens: PLensFamily[S1, S2, (A, B, C), (A, B, C)]):
  (PLensFamily[S1, S2, A, A], PLensFamily[S1, S2, B, B], PLensFamily[S1, S2, C, C]) =
    PLensFamilyUnzip[S1, S2].unzip3(lens.xmapbB(tuple3B))

  def tuple4PLensFamily[S1, S2, A, B, C, D](lens: PLensFamily[S1, S2, (A, B, C, D), (A, B, C, D)]):
  (PLensFamily[S1, S2, A, A], PLensFamily[S1, S2, B, B], PLensFamily[S1, S2, C, C], PLensFamily[S1, S2, D, D]) =
    PLensFamilyUnzip[S1, S2].unzip4(lens.xmapbB(tuple4B))

  def tuple5PLensFamily[S1, S2, A, B, C, D, E](lens: PLensFamily[S1, S2, (A, B, C, D, E), (A, B, C, D, E)]):
  (PLensFamily[S1, S2, A, A], PLensFamily[S1, S2, B, B], PLensFamily[S1, S2, C, C], PLensFamily[S1, S2, D, D], PLensFamily[S1, S2, E, E]) =
    PLensFamilyUnzip[S1, S2].unzip5(lens.xmapbB(tuple5B))

  def tuple6PLensFamily[S1, S2, A, B, C, D, E, H](lens: PLensFamily[S1, S2, (A, B, C, D, E, H), (A, B, C, D, E, H)]):
  (PLensFamily[S1, S2, A, A], PLensFamily[S1, S2, B, B], PLensFamily[S1, S2, C, C], PLensFamily[S1, S2, D, D], PLensFamily[S1, S2, E, E], PLensFamily[S1, S2, H, H]) =
    PLensFamilyUnzip[S1, S2].unzip6(lens.xmapbB(tuple6B))

  def tuple7PLensFamily[S1, S2, A, B, C, D, E, H, I](lens: PLensFamily[S1, S2, (A, B, C, D, E, H, I), (A, B, C, D, E, H, I)]):
  (PLensFamily[S1, S2, A, A], PLensFamily[S1, S2, B, B], PLensFamily[S1, S2, C, C], PLensFamily[S1, S2, D, D], PLensFamily[S1, S2, E, E], PLensFamily[S1, S2, H, H], PLensFamily[S1, S2, I, I]) =
    PLensFamilyUnzip[S1, S2].unzip7(lens.xmapbB(tuple7B))

  def eitherLensFamily[S1, S2, A, B](l: PLensFamily[S1, S2, A \/ B, A \/ B]): (PLensFamily[S1, S2, A, A], PLensFamily[S1, S2, B, B]) =
    (
    leftPLensFamily compose l
    , rightPLensFamily compose l
    )

  import LazyOption._

  def lazySomePLensFamily[A1, A2]: PLensFamily[LazyOption[A1], LazyOption[A2], A1, A2] =
    plensFamily(_.fold(z => Some(IndexedStore(lazySome(_), z)), None))

  import LazyEither._

  def lazyLeftPLensFamily[A1, A2, B]: PLensFamily[LazyEither[A1, B], LazyEither[A2, B], A1, A2] =
    plensFamily(_.fold(a => Some(IndexedStore(lazyLeft(_), a)), _ => None))

  def lazyRightPLensFamily[A, B1, B2]: PLensFamily[LazyEither[A, B1], LazyEither[A, B2], B1, B2] =
    plensFamily(_.fold(_ => None, b => Some(IndexedStore(lazyRight(_), b))))

  def factorPLensFamily[A1, A2, B1, B2, C1, C2]: PLensFamily[((A1, B1) \/ (A1, C1)), ((A2, B2) \/ (A2, C2)), (A1, B1 \/ C1), (A2, B2 \/ C2)] =
    ~LensFamily.factorLensFamily

  def distributePLensFamily[A1, A2, B1, B2, C1, C2]: PLensFamily[(A1, B1 \/ C1), (A2, B2 \/ C2), ((A1, B1) \/ (A1, C1)), ((A2, B2) \/ (A2, C2))] =
    ~LensFamily.distributeLensFamily
}

trait PLensFunctions extends PLensFamilyFunctions with PLensInstances {
  import BijectionT._

  def plens[A, B](r: A => Option[Store[B, A]]): PLens[A, B] = new PLens[A, B] {
    def run(a: A): Option[Store[B, A]] = r(a)
  }

  def plensf[A, B](r: PartialFunction[A, Store[B, A]]): PLens[A, B] =
    plens(r.lift)

  def plensg[A, B](set: A => Option[B => A], get: A => Option[B]): PLens[A, B] =
    plens(a => for {
      w <- set(a)
      x <- get(a)
    } yield Store(w, x))

  def plensgf[A, B](set: PartialFunction[A, B => A], get: PartialFunction[A, B]): PLens[A, B] =
    plensg(set.lift, get.lift)

  /** The identity partial lens for a given object */
  def plensId[A]: PLens[A, A] =
    LensFamily.lensId[A].partial

  /** The trivial partial lens that can retrieve Unit from anything */
  def trivialPLens[A]: PLens[A, Unit] =
    LensFamily.trivialLens[A].partial

  /** A lens that discards the choice of right or left from disjunction */
  def codiagPLens[A]: PLens[A \/ A, A] =
    plensId[A] ||| plensId[A]

  /** The always-null partial lens */
  def nil[A, B]: PLens[A, B] =
    plens(_ => None)

  def somePLens[A]: Option[A] @?> A =
    plens(_ map (z => Store(Some(_), z)))

  def leftPLens[A, B]: (A \/ B) @?> A =
    plens {
      case -\/(a) => Some(Store(-\/(_), a))
      case \/-(_) => None
    }

  def rightPLens[A, B]: (A \/ B) @?> B =
    plens {
      case \/-(b) => Some(Store(\/-(_), b))
      case -\/(_) => None
    }

  def tuple2PLens[S, A, B](lens: PLens[S, (A, B)]):
  (PLens[S, A], PLens[S, B]) =
    PLensFamilyUnzip[S, S].unzip(lens)

  def tuple3PLens[S, A, B, C](lens: PLens[S, (A, B, C)]):
  (PLens[S, A], PLens[S, B], PLens[S, C]) =
    PLensFamilyUnzip[S, S].unzip3(lens.xmapbB(tuple3B))

  def tuple4PLens[ S, A, B, C, D](lens: PLens[S, (A, B, C, D)]):
  (PLens[S, A], PLens[S, B], PLens[S, C], PLens[S, D]) =
    PLensFamilyUnzip[S, S].unzip4(lens.xmapbB(tuple4B))

  def tuple5PLens[S, A, B, C, D, E](lens: PLens[S, (A, B, C, D, E)]):
  (PLens[S, A], PLens[S, B], PLens[S, C], PLens[S, D], PLens[S, E]) =
    PLensFamilyUnzip[S, S].unzip5(lens.xmapbB(tuple5B))

  def tuple6PLens[S, A, B, C, D, E, H](lens: PLens[S, (A, B, C, D, E, H)]):
  (PLens[S, A], PLens[S, B], PLens[S, C], PLens[S, D], PLens[S, E], PLens[S, H]) =
    PLensFamilyUnzip[S, S].unzip6(lens.xmapbB(tuple6B))

  def tuple7PLens[S, A, B, C, D, E, H, I](lens: PLens[S, (A, B, C, D, E, H, I)]):
  (PLens[S, A], PLens[S, B], PLens[S, C], PLens[S, D], PLens[S, E], PLens[S, H], PLens[S, I]) =
    PLensFamilyUnzip[S, S].unzip7(lens.xmapbB(tuple7B))

  def eitherLens[S, A, B](l: S @?> (A \/ B)): (S @?> A, S @?> B) =
    (
    leftPLens compose l
    , rightPLens compose l
    )

  import LazyOption._

  def lazySomePLens[A]: LazyOption[A] @?> A =
    plens(_.fold(z => Some(Store(lazySome(_), z)), None))

  import LazyEither._

  def lazyLeftPLens[A, B]: LazyEither[A, B] @?> A =
    plens(_.fold(a => Some(Store(lazyLeft(_), a)), _ => None))

  def lazyRightPLens[A, B]: LazyEither[A, B] @?> B =
    plens(_.fold(_ => None, b => Some(Store(lazyRight(_), b))))

  def listHeadPLens[A]: List[A] @?> A =
    plens {
      case Nil => None
      case h :: t => Some(Store(_ :: t, h))
    }

  def listTailPLens[A]: List[A] @?> List[A] =
    plens {
      case Nil => None
      case h :: t => Some(Store(h :: _, t))
    }

  def listNthPLens[A](n: Int): List[A] @?> A =
    if(n < 0)
      nil
    else if(n == 0)
      listHeadPLens
    else
      listNthPLens(n - 1) compose listTailPLens

  def listLookupByPLens[K, V](p: K => Boolean): List[(K, V)] @?> V = {
    @annotation.tailrec
    def lookupr(t: (List[(K, V)], (K, V), List[(K, V)])): Option[(List[(K, V)], (K, V), List[(K, V)])] =
      t match {
        case (_, (k, _), _) if p(k) => Some(t)
        case (_, _     , Nil)       => None
        case (l, x     , r::rs)     => lookupr(x::l, r, rs)
      }
    plens {
      case Nil => None
      case h :: t => lookupr(Nil, h, t) map {
        case (l, (k, v), r) => Store(w => l.reverse ::: (k, w) :: r, v)
      }
    }
  }

  def listLookupPLens[K: Equal, V](k: K): List[(K, V)] @?> V =
    listLookupByPLens(Equal[K].equal(k, _))

  def vectorHeadPLens[A]: Vector[A] @?> A =
    vectorNthPLens(0)

  def vectorNthPLens[A](n: Int): Vector[A] @?> A =
    plens(v =>
      v.lift(n) map (a => Store(x => v patch (n, Vector(x), 1), a)))

  def vectorLastPLens[A]: Vector[A] @?> A =
    plens(v =>
      v.lastOption map (a => Store(x => v patch (v.length - 1, Vector(x), 1), a)))

  import Stream._

  def streamHeadPLens[A]: Stream[A] @?> A =
    plens {
      case Empty => None
      case h #:: t => Some(Store(_ #:: t, h))
    }

  def streamTailPLens[A]: Stream[A] @?> Stream[A] =
    plens {
      case Empty => None
      case h #:: t => Some(Store(h #:: _, t))
    }

  def streamNthPLens[A](n: Int): Stream[A] @?> A =
    if(n < 0)
      nil
    else if(n == 0)
      streamHeadPLens
    else
      streamNthPLens(n - 1) compose streamTailPLens

  def streamLookupByPLens[K, V](p: K => Boolean): Stream[(K, V)] @?> V = {
    @annotation.tailrec
    def lookupr(t: (Stream[(K, V)], (K, V), Stream[(K, V)])): Option[(Stream[(K, V)], (K, V), Stream[(K, V)])] =
      t match {
        case (_, (k, _), _) if p(k)    => Some(t)
        case (_, _     , Stream.Empty) => None
        case (l, x     , r #:: rs)     => lookupr(x #:: l, r, rs)
      }
    plens {
      case Stream.Empty => None
      case h #:: t => lookupr(Stream.empty, h, t) map {
        case (l, (k, v), r) => Store(w => l.reverse #::: (k, w) #:: r, v)
      }
    }
  }

  def streamLookupPLens[K: Equal, V](k: K): Stream[(K, V)] @?> V =
    streamLookupByPLens(Equal[K].equal(k, _))

  def ephemeralStreamHeadPLens[A]: EphemeralStream[A] @?> A =
    plens(s =>
      if(s.isEmpty)
        None
      else
        Some(Store(EphemeralStream.cons(_, s.tail()), s.head()))
    )

  def ephemeralStreamTailPLens[A]: EphemeralStream[A] @?> EphemeralStream[A] =
    plens(s =>
      if(s.isEmpty)
        None
      else
        Some(Store(EphemeralStream.cons(s.head(), _), s.tail()))
    )

  def ephemeralStreamNthPLens[A](n: Int): EphemeralStream[A] @?> A =
    if(n < 0)
      nil
    else if(n == 0)
      ephemeralStreamHeadPLens
    else
      ephemeralStreamNthPLens(n - 1) compose ephemeralStreamTailPLens

  def ephemeralStreamLookupByPLens[K, V](p: K => Boolean): EphemeralStream[(K, V)] @?> V = {
    import EphemeralStream.cons

    @annotation.tailrec
    def lookupr(t: (EphemeralStream[(K, V)], (K, V), EphemeralStream[(K, V)])): Option[(EphemeralStream[(K, V)], (K, V), EphemeralStream[(K, V)])] =
      t match {
        case (_, (k, _), _) if p(k)    => Some(t)
        case (l, x     , s) =>
            if(s.isEmpty)
              None
            else
              lookupr((cons(x, l), s.head(), s.tail()))
      }
    plens(s =>
      if(s.isEmpty)
        None
      else
        lookupr((EphemeralStream.emptyEphemeralStream, s.head(), s.tail())) map {
          case (l, (k, v), r) => Store(w => l.reverse ++ cons((k, w), r), v)
        }
    )
  }

  def ephemeralStreamLookupPLens[K: Equal, V](k: K): EphemeralStream[(K, V)] @?> V =
    ephemeralStreamLookupByPLens(Equal[K].equal(k, _))

  import LensFamily.mapVLens

  def mapVPLens[K, V](k: K): Map[K, V] @?> V =
    somePLens compose ~mapVLens[K, V](k)

  def factorPLens[A, B, C]: ((A, B) \/ (A, C)) @?> (A, B \/ C) =
    ~LensFamily.factorLens

  def distributePLens[A, B, C]: (A, B \/ C) @?> ((A, B) \/ (A, C)) =
    ~LensFamily.distributeLens

  import util.parsing.json._

  def scalaJSONObjectPLens: JSONType @?> Map[String, Any] =
    plens {
      case JSONObject(m) => Some(Store(JSONObject(_), m))
      case _             => None
    }

  def scalaJSONArrayPLens: JSONType @?> List[Any] =
    plens {
      case JSONArray(a) => Some(Store(JSONArray(_), a))
      case _            => None
    }
}

trait PLensInstances {

  import PLensFamily._

  implicit def plensCategory: PLensCategory = new PLensCategory {
  }

  /** Partial Lenses may be used implicitly as State monadic actions that get the potentially viewed portion of the state */
  implicit def PLensFamilyState[A, B](plens: PLensFamily[A, _, B, _]): PState[A, B] =
    plens.st

  implicit def PLensFamilyUnzip[S, R]: Unzip[({type λ[α] = PLensFamily[S, R, α, α]})#λ] =
    new Unzip[({type λ[α] = PLensFamily[S, R, α, α]})#λ] {
      def unzip[A, B](a: PLensFamily[S, R, (A, B), (A, B)]) =
        (
          plensFamily(x => a run x map (c => {
            val (p, q) = c.pos
            IndexedStore(a => c.put((a, q)): R, p)
          }))
        , plensFamily(x => a run x map (c => {
            val (p, q) = c.pos
            IndexedStore(a => c.put((p, a)): R, q)
          }))
        )
    }

  /** Allow the illusion of imperative updates to potential numbers viewed through a partial lens */
  case class NumericPLens[S, N: Numeric](lens: S @?> N, num: Numeric[N]) {
    def +=(that: N): PState[S, N] =
      lens %= (num.minus(_, that))

    def -=(that: N): PState[S, N] =
      lens %= (num.minus(_, that))

    def *=(that: N): PState[S, N] =
      lens %= (num.times(_, that))
  }

  implicit def numericPLens[S, N: Numeric](lens: S @?> N) =
    NumericPLens[S, N](lens, implicitly[Numeric[N]])

  /** Allow the illusion of imperative updates to potential numbers viewed through a partial lens */
  case class FractionalPLens[S, F](lens: S @?> F, frac: Fractional[F]) {
    def /=(that: F): PState[S, F] =
      lens %= (frac.div(_, that))
  }

  implicit def fractionalPLens[S, F: Fractional](lens: S @?> F) =
    FractionalPLens[S, F](lens, implicitly[Fractional[F]])

  /** Allow the illusion of imperative updates to potential numbers viewed through a partial lens */
  case class IntegralPLens[S, I](lens: S @?> I, ig: Integral[I]) {
    def %=(that: I): PState[S, I] =
      lens %= (ig.quot(_, that))
  }

  implicit def integralPLens[S, I: Integral](lens: S @?> I) =
    IntegralPLens[S, I](lens, implicitly[Integral[I]])

}

private[scalaz] trait PLensCategory
  extends Choice[PLens]
  with Split[PLens] {

  def compose[A, B, C](bc: PLens[B, C], ab: PLens[A, B]): PLens[A, C] = ab >=> bc
  def id[A]: PLens[A, A] = PLensFamily.plensId

  def choice[A, B, C](f: => PLens[A, C], g: => PLens[B, C]): PLens[A \/ B, C] =
    PLensFamily.plens[A \/ B, C] {
      case -\/(a) =>
        f run a map (_ map (-\/(_)))
      case \/-(b) =>
        g run b map (_ map (\/-(_)))
    }

  def split[A, B, C, D](f: PLens[A, B], g: PLens[C, D]): PLens[(A,  C), (B, D)] =
    f *** g
}
