package scalaz

import CostateT._

sealed trait PLensT[F[_], A, B] {
  def run(a: A): F[Option[Costate[B, A]]]

  def apply(a: A): F[Option[Costate[B, A]]] =
    run(a)

  def runO(a: A): OptionT[F, Costate[B, A]] =
    OptionT(run(a))

  import StateT._
  import PLensT._
  import BijectionT._

  def kleisli: Kleisli[({type λ[α] = OptionT[F, α]})#λ, A, Costate[B, A]] =
    Kleisli[({type λ[α] = OptionT[F, α]})#λ, A, Costate[B, A]](runO(_))

  def mapC[C](f: Costate[B, A] => Costate[C, A])(implicit F: Functor[F]): PLensT[F, A, C] =
    plensT(a => F.map(run(a))(_ map f))

  def xmapA[X](f: A => X, g: X => A)(implicit F: Functor[F]): PLensT[F, X, B] =
    plensO(x => runO(g(x)) map (_ map f))

  def xmapbA[X](b: Bijection[A, X])(implicit F: Functor[F]): PLensT[F, X, B] =
    xmapA(b to _, b fr _)

  def xmapB[X](f: B => X, g: X => B)(implicit F: Functor[F]): PLensT[F, A, X] =
    plensO(a => runO(a) map (_ xmap (f, g)))

  def xmapbB[X](b: Bijection[B, X])(implicit F: Functor[F]): PLensT[F, A, X] =
    xmapB(b to _, b fr _)

  def lift[G[_]](implicit P: Pointed[G], F: Functor[F]): PLensT[({type λ[α] = F[G[α]]})#λ, A, B] =
    plensT[({type λ[α] = F[G[α]]})#λ, A, B](a => F.map(run(a))(P.point(_)))

  def wlift[W](implicit F: Functor[F], M: Monoid[W]): PLenswT[F, W, A, B] =
    plensT[({type λ[α] = WriterT[F, W, α]})#λ, A, B](a =>
          WriterT(F.map(run(a))((M.zero, _))))

  def wliftC[W](z: (A, Option[Costate[B, A]]) => W)(implicit F: Functor[F]): PLenswT[F, W, A, B] =
    plensT[({type λ[α] = WriterT[F, W, α]})#λ, A, B](a =>
          WriterT(F.map(run(a))(x => (z(a, x), x))))

  /** alias for `wliftC` */
  def !|![W](z: (A, Option[Costate[B, A]]) => W)(implicit F: Functor[F]): PLenswT[F, W, A, B] =
    wliftC(z)

  def wliftG[W](z: Option[(A, B)] => W)(implicit F: Functor[F]): PLenswT[F, W, A, B] =
    wliftC((a, c) => z(c map (x => (a, x.pos))))

  /** alias for `wliftG` */
  def !![W](z: Option[(A, B)] => W)(implicit F: Functor[F]): PLenswT[F, W, A, B] =
    wliftG(z)

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

  def set(a: A, b: B)(implicit F: Functor[F]): F[Option[A]] =
    F.map(run(a))(_ map (_.put(b)))

  def setO(a: A, b: B)(implicit F: Functor[F]): OptionT[F, A] =
    OptionT(set(a, b))

  def setK(a: A)(implicit F: Functor[F]): Kleisli[({type λ[α] = OptionT[F, α]})#λ, B, A] =
    Kleisli[({type λ[α] = OptionT[F, α]})#λ, B, A](setO(a, _))

  def setOr(a: A, b: B, d: => A)(implicit F: Functor[F]): F[A] =
    F.map(set(a, b))(_ getOrElse d)

  def setOrZ(a: A, b: B)(implicit M: Monoid[A], F: Functor[F]): F[A] =
    setOr(a, b, M.zero)

  def trySet(a: A)(implicit F: Functor[F]): F[Option[B => A]] =
    F.map(run(a))(_ map (c => c.put(_)))

  def trySetO(a: A)(implicit F: Functor[F]): OptionT[F, B => A] =
    OptionT(trySet(a))

  def trySetK(implicit F: Functor[F]): Kleisli[({type λ[α] = OptionT[F, α]})#λ, A, B => A] =
    Kleisli[({type λ[α] = OptionT[F, α]})#λ, A, B => A](trySetO(_))

  def trySetOr(a: A, d: => B => A)(implicit F: Functor[F]): F[B => A] =
    F.map(trySet(a))(_ getOrElse d)

  def trySetOrZ(a: A)(implicit M: Monoid[B => A], F: Functor[F]): F[B => A] =
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

  def option(implicit M: Pointed[F]): PLensT[F, Option[A], B] =
    plensT {
      case None => M.point(None)
      case Some(w) => M.map(run(w))(_ map (_ map (Some(_))))
    }

  /** An alias for `option`. */
  def unary_!(implicit M: Pointed[F]): PLensT[F, Option[A], B] =
    option

  /** Modify the value viewed through the lens */
  def mod(f: B => B, a: A)(implicit F: Functor[F]): F[A] =
    F.map(run(a)) {
      case None => a
      case Some(w) => w puts f
    }

  def =>=(f: B => B)(implicit F: Functor[F]): A => F[A] =
    mod(f, _)

  def st(implicit F: Functor[F]): PStateT[F, A, B] =
    StateT(s => F.map(get(s))((_, s)))

  def %=(f: B => B)(implicit F: Functor[F]): PStateT[F, A, B] =
    StateT(a => F.map(run(a))(_ match {
      case None => (None, a)
      case Some(w) => {
        val r = f(w.pos)
        (Some(r), w put r)
      }
    }))

  def :=(b: => B)(implicit F: Functor[F]): PStateT[F, A, B] =
    %=(_ => b)

  def %==(f: B => B)(implicit F: Functor[F]): StateT[F, A, Unit] =
    StateT(a =>
      F.map(mod(f, a))(((), _)))

  def %%=[C](s: State[B, C])(implicit F: Functor[F]): PStateT[F, A, C] =
    StateT(a => F.map(run(a))(_ match {
      case None => (None, a)
      case Some(w) => {
        val r = s.run(w.pos): (C, B)
        (Some(r._1), w put r._2)
      }
    }))

  def >-[C](f: B => C)(implicit F: Functor[F]): PStateT[F, A, C] =
    StateT(a => F.map(get(a))(x => (x map f, a)))

  def >>-[C](f: B => StateT[F, A, C])(implicit F: Monad[F]): PStateT[F, A, C] =
    StateT(a => F.bind(get(a))(_ match {
      case None => F.point((None, a))
      case Some(w) =>
        F.map(f(w) apply a) {
          case (x, y) => (Some(x), y)
        }
    }))

  def ->>-[C](f: => StateT[F, A, C])(implicit F: Monad[F]): PStateT[F, A, C] =
    >>-(_ => f)

  /** Lenses can be composed */
  def compose[C](that: PLensT[F, C, A])(implicit F: Monad[F]): PLensT[F, C, B] =
    plensO(c =>
      (that runO c).flatMap (x => {
        val (ac, a) = x.run
        runO(a) map (y => {
          val (ba, b) = y.run
          costate(ac compose ba, b)
        })
      }))

  /** alias for `compose` */
  def <=<[C](that: PLensT[F, C, A])(implicit F: Monad[F]): PLensT[F, C, B] = compose(that)

  def andThen[C](that: PLensT[F, B, C])(implicit F: Monad[F]): PLensT[F, A, C] =
    that compose this

  /** alias for `andThen` */
  def >=>[C](that: PLensT[F, B, C])(implicit F: Monad[F]): PLensT[F, A, C] = andThen(that)

  /** Two lenses that view a value of the same type can be joined */
  def sum[C](that: => PLensT[F, C, B])(implicit F: Functor[F]): PLensT[F, Either[A, C], B] =
    plensT{
      case Left(a) =>
        F.map(run(a))(_ map (_ map (Left(_))))
      case Right(c) =>
        F.map(that run c)(_ map (_ map (Right(_))))
    }

  /** Alias for `sum` */
  def |||[C](that: => PLensT[F, C, B])(implicit F: Functor[F]): PLensT[F, Either[A, C], B] = sum(that)

  /** Two disjoint lenses can be paired */
  def product[C, D](that: PLensT[F, C, D])(implicit F: Bind[F]): PLensT[F, (A, C), (B, D)] =
    plensT {
      case (a, c) =>
        F.map2(run(a), that run c)((x, y) => for {
          q <- x
          r <- y
        } yield q *** r)
    }

  /** alias for `product` */
  def ***[C, D](that: PLensT[F, C, D])(implicit F: Bind[F]): PLensT[F, (A, C), (B, D)] = product(that)

}

object PLensT extends PLensTFunctions with PLensTInstances {
  def apply[F[_], A, B](r: A => F[Option[Costate[B, A]]]): PLensT[F, A, B] =
    plensT(r)
}

object PLens extends PLensTFunctions with PLensTInstances {
  def apply[A, B](r: A => Option[Costate[B, A]]): PLens[A, B] =
    plens(r)
}

trait PLensTFunctions {

  import CostateT._

  type PLens[A, B] =
  PLensT[Id, A, B]

  type @?>[A, B] =
  PLens[A, B]

  type PLenswT[F[_], W, A, B] =
    PLensT[({type λ[α] = WriterT[F, W, α]})#λ, A, B]

  type PLensw[W, A, B] =
    PLenswT[Id, W, A, B]

  type PStateT[F[_], A, B] =
  StateT[F, A, Option[B]]

  type PState[A, B] =
  PStateT[Id, A, B]

  def plensT[F[_], A, B](r: A => F[Option[Costate[B, A]]]): PLensT[F, A, B] = new PLensT[F, A, B] {
    def run(a: A): F[Option[Costate[B, A]]] = r(a)
  }

  def plensO[F[_], A, B](r: A => OptionT[F, Costate[B, A]]): PLensT[F, A, B] =
    plensT(a => r(a).run)

  def plens[A, B](r: A => Option[Costate[B, A]]): PLens[A, B] =
    plensT[Id, A, B](r)

  def plensp[F[_], A, B](r: A => Option[Costate[B, A]])(implicit P: Pointed[F]): PLensT[F, A, B] =
    plensT(a => P.point(r(a)))

  def plensgT[F[_], A, B](set: A => F[Option[B => A]], get: A => F[Option[B]])(implicit M: Bind[F]): PLensT[F, A, B] =
    plensT(a => M.map2(set(a), get(a))((q, r) => for {
      w <- q
      x <- r
    } yield costate(w, x)))

  def plensgO[F[_], A, B](set: A => OptionT[F, B => A], get: A => OptionT[F, B])(implicit M: Bind[F]): PLensT[F, A, B] =
    plensgT(a => set(a).run, a => get(a).run)

  def plensg[A, B](set: A => Option[B => A], get: A => Option[B]): PLens[A, B] =
    plensgT[Id, A, B](set, get)

  /** The identity partial lens for a given object */
  def plensId[F[_], A](implicit P: Pointed[F]): PLensT[F, A, A] =
    LensT.lensId.partial

  /** The trivial partial lens that can retrieve Unit from anything */
  def trivialPLens[F[_], A](implicit P: Pointed[F]): PLensT[F, A, Unit] =
    LensT.trivialLens.partial

  /** A lens that discards the choice of Right or Left from Either */
  def codiagPLens[F[_]: Pointed, A]: PLensT[F, Either[A, A], A] =
    plensId[F, A] ||| plensId[F, A]

  /** The always-null partial lens */
  def nil[F[_]: Pointed, A, B]: PLensT[F, A, B] =
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

  def factorPL[A, B, C]: Either[(A, B), (A, C)] @?> (A, Either[B, C]) =
    ~LensT.factorL

  def distributePL[A, B, C]: (A, Either[B, C]) @?> Either[(A, B), (A, C)] =
    ~LensT.distributeL

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
  import BijectionT._

  implicit def plensTCategory[F[_]](implicit F0: Monad[F]) = new PLensTCategory[F] {
    implicit def F: Monad[F] = F0
  }

  /** Lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def PLensState[F[_], A, B](plens: PLensT[F, A, B])(implicit F: Functor[F]): PStateT[F, A, B] =
    plens.st

  implicit def PLensTUnzip[F[_], S](implicit F: Functor[F]): Unzip[({type λ[α] = PLensT[F, S, α]})#λ] =
    new Unzip[({type λ[α] = PLensT[F, S, α]})#λ] {
      def unzip[A, B](a: PLensT[F, S, (A, B)]) =
        (
          PLensT(x => F.map(a run x)(_ map (c => {
            val (p, q) = c.pos
            costate(a => c.put((a, q)), p)
          })))
          , PLensT(x => F.map(a run x)(_ map (c => {
          val (p, q) = c.pos
          costate(a => c.put((p, a)), q)
        })))
          )
    }

  implicit def Tuple2PLens[F[_]: Functor, S, A, B](lens: PLensT[F, S, (A, B)]):
  (PLensT[F, S, A], PLensT[F, S, B]) =
    PLensTUnzip[F, S].unzip(lens)

  implicit def Tuple3PLens[F[_]: Functor, S, A, B, C](lens: PLensT[F, S, (A, B, C)]):
  (PLensT[F, S, A], PLensT[F, S, B], PLensT[F, S, C]) =
    PLensTUnzip[F, S].unzip3(lens.xmapbB(tuple3B))

  implicit def Tuple4PLens[F[_]: Functor, S, A, B, C, D](lens: PLensT[F, S, (A, B, C, D)]):
  (PLensT[F, S, A], PLensT[F, S, B], PLensT[F, S, C], PLensT[F, S, D]) =
    PLensTUnzip[F, S].unzip4(lens.xmapbB(tuple4B))

  implicit def Tuple5PLens[F[_]: Functor, S, A, B, C, D, E](lens: PLensT[F, S, (A, B, C, D, E)]):
  (PLensT[F, S, A], PLensT[F, S, B], PLensT[F, S, C], PLensT[F, S, D], PLensT[F, S, E]) =
    PLensTUnzip[F, S].unzip5(lens.xmapbB(tuple5B))

  implicit def Tuple6PLens[F[_]: Functor, S, A, B, C, D, E, G](lens: PLensT[F, S, (A, B, C, D, E, G)]):
  (PLensT[F, S, A], PLensT[F, S, B], PLensT[F, S, C], PLensT[F, S, D], PLensT[F, S, E], PLensT[F, S, G]) =
    PLensTUnzip[F, S].unzip6(lens.xmapbB(tuple6B))

  implicit def Tuple7PLens[F[_]: Functor, S, A, B, C, D, E, G, H](lens: PLensT[F, S, (A, B, C, D, E, G, H)]):
  (PLensT[F, S, A], PLensT[F, S, B], PLensT[F, S, C], PLensT[F, S, D], PLensT[F, S, E], PLensT[F, S, G], PLensT[F, S, H]) =
    PLensTUnzip[F, S].unzip7(lens.xmapbB(tuple7B))

  implicit def eitherLens[S, A, B](l: S @?> Either[A, B]): (S @?> A, S @?> B) =
    (
      leftPLens compose l
      , rightPLens compose l
      )

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

private[scalaz] trait PLensTCategory[F[_]] extends
Category[({type λ[α, β] = PLensT[F, α, β]})#λ] with
Choice[({type λ[α, β] = PLensT[F, α, β]})#λ] with
Split[({type λ[α, β] = PLensT[F, α, β]})#λ] with
Codiagonal[({type λ[α, β] = PLensT[F, α, β]})#λ] {
  implicit def F: Monad[F]

  def compose[A, B, C](bc: PLensT[F, B, C], ab: PLensT[F, A, B]): PLensT[F, A, C] = ab >=> bc

  def id[A] = PLensT.plensId

  def choice[A, B, C](f: => PLensT[F, A, C], g: => PLensT[F, B, C]): PLensT[F, Either[A, B], C] =
    PLensT.plensT {
      case Left(a) =>
        F.map(f run a)(_ map (_ map (Left(_))))
      case Right(b) =>
        F.map(g run b)(_ map (_ map (Right(_))))
    }

  def split[A, B, C, D](f: PLensT[F, A, B], g: PLensT[F, C, D]): PLensT[F, (A,  C), (B, D)] =
    f *** g

  def codiagonal[A]: PLensT[F, Either[A,  A], A] =
    PLensT.codiagPLens[F, A]

}