package scalaz

import CostateT._

sealed trait PLensT[F[_], G[_], A, B] {
  def run(a: A): F[Option[Costate[B, G[A]]]]

  def apply(a: A): F[Option[Costate[B, G[A]]]] =
    run(a)

  def runO(a: A): OptionT[F, Costate[B, G[A]]] =
    OptionT(run(a))

  import StateT._
  import PLensT._
  import BijectionT._

  def kleisli: Kleisli[({type λ[α] = OptionT[F, α]})#λ, A, Costate[B, G[A]]] =
    Kleisli[({type λ[α] = OptionT[F, α]})#λ, A, Costate[B, G[A]]](runO(_))

  def mapC[C](f: Costate[B, G[A]] => Costate[C, G[A]])(implicit FF: Functor[F]): PLensT[F, G, A, C] =
    plensT(a => FF.map(run(a))(_ map f))

  def xmapA[X](f: A => X, g: X => A)(implicit FF: Functor[F], GF: Functor[G]): PLensT[F, G, X, B] =
    plensO(x => runO(g(x)) map (_ map (GF.map(_)(f))))

  def xmapbA[X](b: Bijection[A, X])(implicit FF: Functor[F], GF: Functor[G]): PLensT[F, G, X, B] =
    xmapA(b to _, b fr _)

  def xmapB[X](f: B => X, g: X => B)(implicit FF: Functor[F], GF: Functor[G]): PLensT[F, G, A, X] =
    plensO(a => runO(a) map (_ xmap (f, g)))

  def xmapbB[X](b: Bijection[B, X])(implicit FF: Functor[F], GF: Functor[G]): PLensT[F, G, A, X] =
    xmapB(b to _, b fr _)

  def lift[X[_]](implicit P: Pointed[X], FF: Functor[F], GF: Functor[G]): PLensT[({type λ[α] = F[X[α]]})#λ, ({type λ[α] = G[X[α]]})#λ, A, B] =
    plensT[({type λ[α] = F[X[α]]})#λ, ({type λ[α] = G[X[α]]})#λ, A, B](a => FF.map(run(a))(c => P.point(c map (_ map (GF.map(_)(P.point(_)))))))

  def wlift[V, W](implicit FF: Functor[F], GF: Functor[G], MV: Monoid[V], MW: Monoid[W]): PLenswT[F, G, V, W, A, B] =
    plensT[({type λ[α] = WriterT[F, V, α]})#λ, ({type λ[α] = WriterT[G, W, α]})#λ, A, B](a =>
          WriterT(FF.map(run(a))(e => (MV.zero, e map (_ map (q => WriterT(GF.map(q)((MW.zero, _)))))))))

  def wrunlift[V, W](getV: (A, Option[B]) => V, set: (A, B, A) => W)(implicit FF: Functor[F], GF: Functor[G]): PLenswT[F, G, V, W, A, B] =
    plensT[({type λ[α] = WriterT[F, V, α]})#λ, ({type λ[α] = WriterT[G, W, α]})#λ, A, B](a =>
      WriterT(FF.map(run(a))(e =>
        (getV(a, e map (_.pos)), e map (z => costate(x => WriterT(GF.map(z put x)(q => (set(a, x, q), q))), z.pos)))
      )))

  def wrunliftg[V, W](getV: (A, Option[B]) => V)(implicit FF: Functor[F], GF: Functor[G], MW: Monoid[W]): PLenswT[F, G, V, W, A, B] =
    wrunlift(getV, (_, _, _) => MW.zero)

  def !![W](getV: (A, Option[B]) => W)(implicit FF: Functor[F], GF: Functor[G], MW: Monoid[W]): PLenswT[F, G, W, W, A, B] =
    wrunliftg(getV)

  def wrunlifts[V, W](setV: (A, B, A) => W)(implicit FF: Functor[F], GF: Functor[G], MV: Monoid[V]): PLenswT[F, G, V, W, A, B] =
    wrunlift((_, _) => MV.zero, setV)

  def !|![V](setV: (A, B, A) => V)(implicit FF: Functor[F], GF: Functor[G], MV: Monoid[V]): PLenswT[F, G, V, V, A, B] =
    wrunlifts(setV)

  def get(a: A)(implicit F: Functor[F]): F[Option[B]] =
    F.map(run(a))(_ map (_.pos))

  def getO(a: A)(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(get(a))

  def getK(implicit F: Functor[F]): Kleisli[({type λ[α] = OptionT[F, α]})#λ, A, B] =
    Kleisli[({type λ[α] = OptionT[F, α]})#λ, A, B](getO(_))

  /** If the PartialLens is null, then return the given default value. */
  def getOr(a: A, b: => B)(implicit F: Functor[F]): F[B] =
    F.map(get(a))(_ getOrElse b)

  def getOrZ(a: A)(implicit M: Monoid[B], F: Functor[F]): F[B] =
    getOr(a, M.zero)

  def set(a: A, b: B)(implicit F: Functor[F]): F[Option[G[A]]] =
    F.map(run(a))(_ map (_.put(b)))

  def setO(a: A, b: B)(implicit F: Functor[F]): OptionT[F, G[A]] =
    OptionT(set(a, b))

  def setK(a: A)(implicit F: Functor[F]): Kleisli[({type λ[α] = OptionT[F, α]})#λ, B, G[A]] =
    Kleisli[({type λ[α] = OptionT[F, α]})#λ, B, G[A]](setO(a, _))

  def setOr(a: A, b: B, d: => G[A])(implicit F: Functor[F]): F[G[A]] =
    F.map(set(a, b))(_ getOrElse d)

  def setOrZ(a: A, b: B)(implicit M: Monoid[G[A]], F: Functor[F]): F[G[A]] =
    setOr(a, b, M.zero)

  def trySet(a: A)(implicit F: Functor[F]): F[Option[Kleisli[G, B, A]]] =
    F.map(run(a))(_ map (c => Kleisli[G, B, A](c put _)))

  def trySetO(a: A)(implicit F: Functor[F]): OptionT[F, Kleisli[G, B, A]] =
    OptionT(trySet(a))

  def trySetK(implicit F: Functor[F]): Kleisli[({type λ[α] = OptionT[F, α]})#λ, A, Kleisli[G, B, A]] =
    Kleisli[({type λ[α] = OptionT[F, α]})#λ, A, Kleisli[G, B, A]](trySetO(_))

  def trySetOr(a: A, d: => Kleisli[G, B, A])(implicit F: Functor[F]): F[Kleisli[G, B, A]] =
    F.map(trySet(a))(_ getOrElse d)

  def trySetOrZ(a: A)(implicit M: Monoid[Kleisli[G, B, A]], F: Functor[F]): F[Kleisli[G, B, A]] =
    trySetOr(a, M.zero)

  /** If the PartialLens is null, then return the target object, otherwise run the function on its projection. */
  def as(f: B => A, a: A)(implicit F: Functor[F]): F[A] =
    F.map(get(a))(_ match {
      case None => a
      case Some(w) => f(w)
    })

  def is(a: A)(implicit F: Functor[F]): F[Boolean] =
    F.map(run(a))(_.isDefined)

  def isNot(a: A)(implicit F: Functor[F]): F[Boolean] =
    F.map(is(a))(!_)

  def exists(p: B => Boolean, a: A)(implicit F: Functor[F]): F[Boolean] =
    F.map(get(a))(_ exists p)

  def forall(p: B => Boolean, a: A)(implicit F: Functor[F]): F[Boolean] =
    F.map(get(a))(_ forall p)

  def option(implicit M: Pointed[F], GF: Functor[G]): PLensT[F, G, Option[A], B] =
    plensT {
      case None => M.point(None)
      case Some(w) => M.map(run(w))(_ map (_ map (GF.map(_)(Some(_)))))
    }

  /** An alias for `option`. */
  def unary_!(implicit M: Pointed[F], GF: Functor[G]): PLensT[F, G, Option[A], B] =
    option

  /** Modify the value viewed through the lens */
  def mod(f: B => B, a: A)(implicit F: Functor[F], ev: G[A] =:= Id[A]): F[A] =
    F.map(run(a)){
      case None => a
      case Some(w) => w puts f
    }

  def =>=(f: B => B)(implicit F: Functor[F], ev: G[A] =:= Id[A]): A => F[A] =
    mod(f, _)

  def st(implicit F: Functor[F]): PStateT[F, A, B] =
    StateT(s => F.map(get(s))((_, s)))

  def %=(f: B => B)(implicit F: Functor[F], ev: G[A] =:= Id[A]): PStateT[F, A, B] =
    StateT(a => F.map(run(a))(_ match {
      case None => (None, a)
      case Some(w) => {
        val r = f(w.pos)
        (Some(r), w put r)
      }
    }))

  def :=(b: => B)(implicit F: Functor[F], ev: G[A] =:= Id[A]): PStateT[F, A, B] =
    %=(_ => b)

  def %==(f: B => B)(implicit F: Functor[F], ev: G[A] =:= Id[A]): StateT[F, A, Unit] =
    StateT(a =>
      F.map(mod(f, a))(((), _)))

  def %%=[C](s: State[B, C])(implicit F: Functor[F], ev: G[A] =:= Id[A]): PStateT[F, A, C] =
    StateT(a => F.map(run(a))(_ match {
      case None => (None, a)
      case Some(w) => {
        val r = s.run(w.pos): (C, B)
        (Some(r._1), w put r._2)
      }
    }))

  def >-[C](f: B => C)(implicit F: Functor[F], ev: G[A] =:= Id[A]): PStateT[F, A, C] =
    StateT(a => F.map(get(a))(x => (x map f, a)))

  def >>-[C](f: B => StateT[F, A, C])(implicit F: Monad[F], ev: G[A] =:= Id[A]): PStateT[F, A, C] =
    StateT(a => F.bind(get(a))(_ match {
      case None => F.point((None, a))
      case Some(w) =>
        F.map(f(w) apply a) {
          case (x, y) => (Some(x), y)
        }
    }))

  def ->>-[C](f: => StateT[F, A, C])(implicit F: Monad[F], ev: G[A] =:= Id[A]): PStateT[F, A, C] =
    >>-(_ => f)

  /** Lenses can be composed */
  def compose[C](that: PLensT[F, G, C, A])(implicit FF: Monad[F], GF: Monad[G]): PLensT[F, G, C, B] =
    plensO(c =>
      (that runO c).flatMap (x => {
        val (ac, a) = x.run
        runO(a) map (y => {
          val (ba, b) = y.run
          costate(x => GF.bind(ba(x))(ac), b)
        })
      }))

  /** alias for `compose` */
  def <=<[C](that: PLensT[F, G, C, A])(implicit FF: Monad[F], GF: Monad[G]): PLensT[F, G, C, B] = compose(that)

  def andThen[C](that: PLensT[F, G, B, C])(implicit FF: Monad[F], GF: Monad[G]): PLensT[F, G, A, C] =
    that compose this

  /** alias for `andThen` */
  def >=>[C](that: PLensT[F, G, B, C])(implicit FF: Monad[F], GF: Monad[G]): PLensT[F, G, A, C] = andThen(that)

  /** Two lenses that view a value of the same type can be joined */
  def sum[C](that: => PLensT[F, G, C, B])(implicit FF: Functor[F], GF: Functor[G]): PLensT[F, G, Either[A, C], B] =
    plensT{
      case Left(a) =>
        FF.map(run(a))(_ map (_ map (GF.map(_)(Left(_)))))
      case Right(c) =>
        FF.map(that run c)(_ map (_ map (GF.map(_)(Right(_)))))
    }

  /** Alias for `sum` */
  def |||[C](that: => PLensT[F, G, C, B])(implicit FF: Functor[F], GF: Functor[G]): PLensT[F, G, Either[A, C], B] = sum(that)

  /** Two disjoint lenses can be paired */
  def product[C, D](that: PLensT[F, G, C, D])(implicit FF: Apply[F], GG: Apply[G]): PLensT[F, G, (A, C), (B, D)] =
    plensT {
      case (a, c) =>
        FF.map2(run(a), that run c)((x, y) => for {
          q <- x
          r <- y
        } yield q *** r map {
          case (s, t) => GG.map2(s, t)((i, j) => (i, j))
        })
    }

  /** alias for `product` */
  def ***[C, D](that: PLensT[F, G, C, D])(implicit FF: Apply[F], GG: Apply[G]): PLensT[F, G, (A, C), (B, D)] = product(that)

}

object PLensT extends PLensTFunctions with PLensTInstances {
  def apply[F[_], G[_], A, B](r: A => F[Option[Costate[B, G[A]]]]): PLensT[F, G, A, B] =
    plensT(r)
}

object PLens extends PLensTFunctions with PLensTInstances {
  def apply[A, B](r: A => Option[Costate[B, A]]): PLens[A, B] =
    plens(r)
}

trait PLensTFunctions extends PLensTInstances {

  import CostateT._
  import BijectionT._

  type PLens[A, B] =
  PLensT[Id, Id, A, B]

  type @?>[A, B] =
  PLens[A, B]

  type PLenswT[F[_], G[_], V, W, A, B] =
    PLensT[({type λ[α] = WriterT[F, V, α]})#λ, ({type λ[α] = WriterT[G, W, α]})#λ, A, B]

  type PLensw[V, W, A, B] =
    PLenswT[Id, Id, V, W, A, B]

  type PStateT[F[_], A, B] =
  StateT[F, A, Option[B]]

  type PState[A, B] =
  PStateT[Id, A, B]

  def plensT[F[_], G[_], A, B](r: A => F[Option[Costate[B, G[A]]]]): PLensT[F, G, A, B] = new PLensT[F, G, A, B] {
    def run(a: A): F[Option[Costate[B, G[A]]]] = r(a)
  }

  def plensO[F[_], G[_], A, B](r: A => OptionT[F, Costate[B, G[A]]]): PLensT[F, G, A, B] =
    plensT(a => r(a).run)

  def plens[A, B](r: A => Option[Costate[B, A]]): PLens[A, B] =
    plensT[Id, Id, A, B](r)

  def plensp[F[_], G[_], A, B](r: A => Option[Costate[B, A]])(implicit PF: Pointed[F], PG: Pointed[G]): PLensT[F, G, A, B] =
    plensT(a => PF.point(r(a) map (_ map  (PG.point(_)))))

  def plensgT[F[_], G[_], A, B](set: A => F[Option[B => G[A]]], get: A => F[Option[B]])(implicit M: Bind[F]): PLensT[F, G, A, B] =
    plensT(a => M.map2(set(a), get(a))((q, r) => for {
      w <- q
      x <- r
    } yield costate(w, x)))

  def plensgO[F[_], G[_], A, B](set: A => OptionT[F, B => G[A]], get: A => OptionT[F, B])(implicit M: Bind[F]): PLensT[F, G, A, B] =
    plensgT(a => set(a).run, a => get(a).run)

  def plensg[A, B](set: A => Option[B => A], get: A => Option[B]): PLens[A, B] =
    plensgT[Id, Id, A, B](set, get)

  /** The identity partial lens for a given object */
  def plensId[F[_], G[_], A](implicit FF: Pointed[F], GF: Pointed[G]): PLensT[F, G, A, A] =
    LensT.lensId[F, G, A].partial

  /** The trivial partial lens that can retrieve Unit from anything */
  def trivialPLens[F[_], G[_], A](implicit FF: Pointed[F], GF: Pointed[G]): PLensT[F, G, A, Unit] =
    LensT.trivialLens[F, G, A].partial

  /** A lens that discards the choice of Right or Left from Either */
  def codiagPLens[F[_]: Pointed, G[_]: Pointed, A]: PLensT[F, G, Either[A, A], A] =
    plensId[F, G, A] ||| plensId[F, G, A]

  /** The always-null partial lens */
  def nil[F[_]: Pointed, G[_]: Pointed, A, B]: PLensT[F, G, A, B] =
    plensp(_ => None)

  def somePLens[A]: Option[A] @?> A =
    plens(_ map (z => costate(Some(_), z)))

  def leftPLens[A, B]: Either[A, B] @?> A =
    plens {
      case Left(a) => Some(costate(Left(_), a))
      case Right(_) => None
    }

  def rightPLens[A, B]: Either[A, B] @?> B =
    plens {
      case Right(b) => Some(costate(Right(_), b))
      case Left(_) => None
    }

  def tuple2PLens[F[_]: Functor, G[_]: Functor, S, A, B](lens: PLensT[F, G, S, (A, B)]):
  (PLensT[F, G, S, A], PLensT[F, G, S, B]) =
    PLensTUnzip[F, G, S].unzip(lens)

  def tuple3PLens[F[_]: Functor, G[_]: Functor, S, A, B, C](lens: PLensT[F, G, S, (A, B, C)]):
  (PLensT[F, G, S, A], PLensT[F, G, S, B], PLensT[F, G, S, C]) =
    PLensTUnzip[F, G, S].unzip3(lens.xmapbB(tuple3B))

  def tuple4PLens[F[_]: Functor, G[_]: Functor, S, A, B, C, D](lens: PLensT[F, G, S, (A, B, C, D)]):
  (PLensT[F, G, S, A], PLensT[F, G, S, B], PLensT[F, G, S, C], PLensT[F, G, S, D]) =
    PLensTUnzip[F, G, S].unzip4(lens.xmapbB(tuple4B))

  def tuple5PLens[F[_]: Functor, G[_]: Functor, S, A, B, C, D, E](lens: PLensT[F, G, S, (A, B, C, D, E)]):
  (PLensT[F, G, S, A], PLensT[F, G, S, B], PLensT[F, G, S, C], PLensT[F, G, S, D], PLensT[F, G, S, E]) =
    PLensTUnzip[F, G, S].unzip5(lens.xmapbB(tuple5B))

  def tuple6PLens[F[_]: Functor, G[_]: Functor, S, A, B, C, D, E, H](lens: PLensT[F, G, S, (A, B, C, D, E, H)]):
  (PLensT[F, G, S, A], PLensT[F, G, S, B], PLensT[F, G, S, C], PLensT[F, G, S, D], PLensT[F, G, S, E], PLensT[F, G, S, H]) =
    PLensTUnzip[F, G, S].unzip6(lens.xmapbB(tuple6B))

  def tuple7PLens[F[_]: Functor, G[_]: Functor, S, A, B, C, D, E, H, I](lens: PLensT[F, G, S, (A, B, C, D, E, H, I)]):
  (PLensT[F, G, S, A], PLensT[F, G, S, B], PLensT[F, G, S, C], PLensT[F, G, S, D], PLensT[F, G, S, E], PLensT[F, G, S, H], PLensT[F, G, S, I]) =
    PLensTUnzip[F, G, S].unzip7(lens.xmapbB(tuple7B))

  def eitherLens[S, A, B](l: S @?> Either[A, B]): (S @?> A, S @?> B) =
    (
    leftPLens compose l
    , rightPLens compose l
    )

  import LazyOption._

  def lazySomePLens[A]: LazyOption[A] @?> A =
    plens(_.fold(z => Some(costate(lazySome(_), z)), None))

  import LazyEither._

  def lazyLeftPLens[A, B]: LazyEither[A, B] @?> A =
    plens(_.fold(a => Some(costate(lazyLeft(_), a)), _ => None))

  def lazyRightPLens[A, B]: LazyEither[A, B] @?> B =
    plens(_.fold(_ => None, b => Some(costate(lazyRight(_), b))))

  def listHeadPLens[A]: List[A] @?> A =
    plens {
      case Nil => None
      case h :: t => Some(costate(_ :: t, h))
    }

  def listTailPLens[A]: List[A] @?> List[A] =
    plens {
      case Nil => None
      case h :: t => Some(costate(h :: _, t))
    }

  def listNthPLens[A](n: Int): List[A] @?> A =
    if(n < 0)
      nil
    else if(n == 0)
      listHeadPLens
    else
      listNthPLens(n - 1) compose listTailPLens

  def vectorHeadPLens[A]: Vector[A] @?> A =
    vectorNthPLens(0)

  def vectorNthPLens[A](n: Int): Vector[A] @?> A =
    plens(v =>
      v.lift(n) map (a => costate(x => v patch (n, Vector(x), 1), a)))

  def vectorLastPLens[A]: Vector[A] @?> A =
    plens(v =>
      v.lastOption map (a => costate(x => v patch (v.length - 1, Vector(x), 1), a)))

  import Stream._

  def streamHeadPLens[A]: Stream[A] @?> A =
    plens {
      case Empty => None
      case h #:: t => Some(costate(_ #:: t, h))
    }

  def streamTailPLens[A]: Stream[A] @?> Stream[A] =
    plens {
      case Empty => None
      case h #:: t => Some(costate(h #:: _, t))
    }

  def streamNthPLens[A](n: Int): Stream[A] @?> A =
    if(n < 0)
      nil
    else if(n == 0)
      streamHeadPLens
    else
      streamNthPLens(n - 1) compose streamTailPLens

  def ephemeralStreamHeadPLens[A]: EphemeralStream[A] @?> A =
    plens(s =>
      if(s.isEmpty)
        None
      else
        Some(costate(EphemeralStream.cons(_, s.tail()), s.head()))
    )

  def ephemeralStreamTailPLens[A]: EphemeralStream[A] @?> EphemeralStream[A] =
    plens(s =>
      if(s.isEmpty)
        None
      else
        Some(costate(EphemeralStream.cons(s.head(), _), s.tail()))
    )

  def ephemeralStreamNthPLens[A](n: Int): EphemeralStream[A] @?> A =
    if(n < 0)
      nil
    else if(n == 0)
      ephemeralStreamHeadPLens
    else
      ephemeralStreamNthPLens(n - 1) compose ephemeralStreamTailPLens

  import LensT.mapVLens

  def mapVPLens[K, V](k: K): Map[K, V] @?> V =
    somePLens compose ~mapVLens[K, V](k)

  def factorPLens[A, B, C]: Either[(A, B), (A, C)] @?> (A, Either[B, C]) =
    ~LensT.factorLens

  def distributePLens[A, B, C]: (A, Either[B, C]) @?> Either[(A, B), (A, C)] =
    ~LensT.distributeLens

  import util.parsing.json._

  def scalaJSONObjectPLens[A]: JSONType @?> Map[String, Any] =
    plens {
      case JSONObject(m) => Some(costate(JSONObject(_), m))
      case _             => None
    }

  def scalaJSONArrayPLens[A]: JSONType @?> List[Any] =
    plens {
      case JSONArray(a) => Some(costate(JSONArray(_), a))
      case _            => None
    }
}

trait PLensTInstances {
  import PLensT._

  implicit def plensTCategory[F[_], G[_]](implicit F0: Monad[F], G0: Monad[G]) = new PLensTCategory[F, G] {
    implicit def F: Monad[F] = F0
    implicit def G: Monad[G] = G0
  }

  /** Lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def PLensState[F[_], G[_], A, B](plens: PLensT[F, G, A, B])(implicit F: Functor[F]): PStateT[F, A, B] =
    plens.st

  implicit def PLensTUnzip[F[_], G[_], S](implicit FF: Functor[F], GF: Functor[G]): Unzip[({type λ[α] = PLensT[F, G, S, α]})#λ] =
    new Unzip[({type λ[α] = PLensT[F, G, S, α]})#λ] {
      def unzip[A, B](a: PLensT[F, G, S, (A, B)]) =
        (
          PLensT(x => FF.map(a run x)(_ map (c => {
            val (p, q) = c.pos
            costate(a => c.put((a, q)): G[S], p)
          })))
          , PLensT(x => FF.map(a run x)(_ map (c => {
          val (p, q) = c.pos
          costate(a => c.put((p, a)): G[S], q)
        })))
          )
    }

  /** Allow the illusion of imperative updates to numbers viewed through a partial lens */
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

  /** Allow the illusion of imperative updates to numbers viewed through a partial lens */
  case class FractionalPLens[S, F](lens: S @?> F, frac: Fractional[F]) {
    def /=(that: F): PState[S, F] =
      lens %= (frac.div(_, that))
  }

  implicit def fractionalPLens[S, F: Fractional](lens: S @?> F) =
    FractionalPLens[S, F](lens, implicitly[Fractional[F]])

  /** Allow the illusion of imperative updates to numbers viewed through a partial lens */
  case class IntegralPLens[S, I](lens: S @?> I, ig: Integral[I]) {
    def %=(that: I): PState[S, I] =
      lens %= (ig.quot(_, that))
  }

  implicit def integralPLens[S, I: Integral](lens: S @?> I) =
    IntegralPLens[S, I](lens, implicitly[Integral[I]])
}

// TODO break this up, so that we can provide instances when less than Monad[F]/Monad[G] is available.
private[scalaz] trait PLensTCategory[F[_], G[_]] extends
Category[({type λ[α, β] = PLensT[F, G, α, β]})#λ] with
Choice[({type λ[α, β] = PLensT[F, G, α, β]})#λ] with
Split[({type λ[α, β] = PLensT[F, G, α, β]})#λ] with
Codiagonal[({type λ[α, β] = PLensT[F, G, α, β]})#λ] {
  implicit def F: Monad[F]
  implicit def G: Monad[G]

  def compose[A, B, C](bc: PLensT[F, G, B, C], ab: PLensT[F, G, A, B]): PLensT[F, G, A, C] = ab >=> bc

  def id[A] = PLensT.plensId

  def choice[A, B, C](f: => PLensT[F, G, A, C], g: => PLensT[F, G, B, C]): PLensT[F, G, Either[A, B], C] =
    PLensT.plensT[F, G, Either[A, B], C] {
      case Left(a) =>
        F.map(f run a)(_ map (_ map (G.map(_)(Left(_)))))
      case Right(b) =>
        F.map(g run b)(_ map (_ map (G.map(_)(Right(_)))))
    }

  def split[A, B, C, D](f: PLensT[F, G, A, B], g: PLensT[F, G, C, D]): PLensT[F, G, (A,  C), (B, D)] =
    f *** g

  def codiagonal[A]: PLensT[F, G, Either[A,  A], A] =
    PLensT.codiagPLens[F, G, A]

}