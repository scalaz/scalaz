package scalaz

import StoreT._
import Leibniz._
import Id._

/**
 * Partial Lens Families, offering a purely functional means to access and retrieve
 * an optional field transitioning from type `B1` to type `B2` in a record that is
 * simultaneously transitioning from type `A1` to type `A2`.  [[scalaz.PLens]] is a
 * convenient alias for when `A1 =:= A2` and `B1 =:= B2`.
 *
 * The term ''field'' should not be interpreted restrictively to mean a member of a class. For example, a partial lens
 * family can address the nth element of a `List`.
 *
 * @see [[scalaz.Lens]]
 *
 * @tparam F Type constructor used to potentially address the optional field
 * @tparam A1 The initial type of the record
 * @tparam A2 The final type of the record
 * @tparam B1 The initial type of the optional field
 * @tparam B2 The final type of the optional field
 */
sealed trait PLensFamilyT[F[+_], -A1, +A2, +B1, -B2] {
  def run(a: A1): F[Option[IndexedStore[B1, B2, A2]]]

  def apply(a: A1): F[Option[IndexedStore[B1, B2, A2]]] =
    run(a)

  def runO(a: A1): OptionT[F, IndexedStore[B1, B2, A2]] =
    OptionT(run(a))

  import StateT._
  import PLensFamilyT._
  import BijectionT._

  def kleisli: Kleisli[({type λ[+α] = OptionT[F, α]})#λ, A1, IndexedStore[B1, B2, A2]] =
    Kleisli[({type λ[+α] = OptionT[F, α]})#λ, A1, IndexedStore[B1, B2, A2]](runO(_))

  def mapC[C1, C2](f: IndexedStore[B1, B2, A2] => IndexedStore[C1, C2, A2])(implicit F: Functor[F]): PLensFamilyT[F, A1, A2, C1, C2] =
    plensFamilyT(a => F.map(run(a))(_ map f))

  def mapCO[C1, C2](f: Option[IndexedStore[B1, B2, A2]] => Option[IndexedStore[C1, C2, A2]])(implicit F: Functor[F]): PLensFamilyT[F, A1, A2, C1, C2] =
    plensFamilyT(a => F.map(run(a))(f))

  def xmapA[X1, X2](f: A2 => X2)(g: X1 => A1)(implicit F: Functor[F]): PLensFamilyT[F, X1, X2, B1, B2] =
    plensFamilyO(x => runO(g(x)) map (_ map (f)))
    

  def xmapbA[X, A >: A2 <: A1](b: Bijection[A, X])(implicit F: Functor[F]): PLensFamilyT[F, X, X, B1, B2] =
    xmapA(b to _)(b from _)

  def xmapB[X1, X2](f: B1 => X1)(g: X2 => B2)(implicit F: Functor[F]): PLensFamilyT[F, A1, A2, X1, X2] =
    plensFamilyO(a => runO(a) map (_.xmap(f)(g)))

  def xmapbB[X, B >: B1 <: B2](b: Bijection[B, X])(implicit F: Functor[F]): PLensFamilyT[F, A1, A2, X, X] =
    xmapB(b to _)(b from _)

  def get(a: A1)(implicit F: Functor[F]): F[Option[B1]] =
    F.map(run(a))(_ map (_.pos))

  def getO(a: A1)(implicit F: Functor[F]): OptionT[F, B1] =
    OptionT(get(a))

  def getK(implicit F: Functor[F]): Kleisli[({type λ[+α] = OptionT[F, α]})#λ, A1, B1] =
    Kleisli[({type λ[+α] = OptionT[F, α]})#λ, A1, B1](getO(_))

  /** If the Partial Lens is null, then return the given default value. */
  def getOr[B >: B1](a: A1, b: => B)(implicit F: Functor[F]): F[B] =
    F.map(get(a))(_ getOrElse b)

  def getOrZ[B >: B1](a: A1)(implicit M: Monoid[B], F: Functor[F]): F[B] =
    getOr(a, M.zero)

  def set(a: A1, b: B2)(implicit F: Functor[F]): F[Option[A2]] =
    F.map(run(a))(_ map (_.put(b)))

  def setO(a: A1, b: B2)(implicit F: Functor[F]): OptionT[F, A2] =
    OptionT(set(a, b))

  def setK(a: A1)(implicit F: Functor[F]): Kleisli[({type λ[+α] = OptionT[F, α]})#λ, B2, A2] =
    Kleisli[({type λ[+α] = OptionT[F, α]})#λ, B2, A2](setO(a, _))

  def setOr[A >: A2](a: A1, b: B2, d: => A)(implicit F: Functor[F]): F[A] =
    F.map(set(a, b))(_ getOrElse d)

  def setOrZ[A >: A2](a: A1, b: B2)(implicit M: Monoid[A], F: Functor[F]): F[A] =
    setOr(a, b, M.zero)

  def trySet(a: A1)(implicit F: Functor[F]): F[Option[B2 => A2]] =
    F.map(run(a))(_ map (c => c put _))

  def trySetO(a: A1)(implicit F: Functor[F]): OptionT[F, B2 => A2] =
    OptionT(trySet(a))

  def trySetK(implicit F: Functor[F]): Kleisli[({type λ[+α] = OptionT[F, α]})#λ, A1, B2 => A2] =
    Kleisli[({type λ[+α] = OptionT[F, α]})#λ, A1, B2 => A2](trySetO(_))

  def trySetOr[A >: A2, B <: B2](a: A1, d: => B => A)(implicit F: Functor[F]): F[B => A] =
    F.map(trySet(a))(_ getOrElse d)

  def trySetOrZ[A >: A2, B <: B2](a: A1)(implicit M: Monoid[B => A], F: Functor[F]): F[B => A] =
    trySetOr(a, M.zero)

  /** If the Partial Lens is null, then return the target object, otherwise run the function on its projection. */
  def as[A <: A1](f: B1 => A, a: A)(implicit F: Functor[F]): F[A] =
    F.map(get(a))(_ match {
      case None => a
      case Some(w) => f(w)
    })

  def is(a: A1)(implicit F: Functor[F]): F[Boolean] =
    F.map(run(a))(_.isDefined)

  def isNot(a: A1)(implicit F: Functor[F]): F[Boolean] =
    F.map(is(a))(!_)

  def exists(p: B1 => Boolean, a: A1)(implicit F: Functor[F]): F[Boolean] =
    F.map(get(a))(_ exists p)

  def forall(p: B1 => Boolean, a: A1)(implicit F: Functor[F]): F[Boolean] =
    F.map(get(a))(_ forall p)

  def modg(f: B1 => B2, a: A1)(implicit F: Functor[F]): F[Option[A2]] =
    F.map(run(a))(_.map(_ puts f))

  def =?>=(f: B1 => B2)(implicit F: Functor[F]): A1 => F[Option[A2]] =
    modg(f, _)

  def modgO(f: B1 => B2, a: A1)(implicit F: Functor[F]): OptionT[F, A2] =
    OptionT(modg(f, a))

  def =??>=(f: B1 => B2)(implicit F: Functor[F]): A1 => OptionT[F, A2] =
    modgO(f, _)

  /** Modify the potential value viewed through the partial lens */
  def mod[A >: A2 <: A1](f: B1 => B2, a: A)(implicit F: Functor[F]): F[A] =
    F.map(run(a)){
      case None => a: A
      case Some(w) => (w puts f): A
    }

  def =>=[A >: A2 <: A1](f: B1 => B2)(implicit F: Functor[F]): A => F[A] =
    mod(f, _)

  def st(implicit F: Functor[F]): PStateT[F, A1, B1] =
    StateT(s => F.map(get(s))((s, _)))

  def %=[A >: A2 <: A1, B >: B1 <: B2](f: B1 => B)(implicit F: Functor[F]): PStateT[F, A, B] =
    StateT(a => F.map(run(a))(_ match {
      case None => (a, None)
      case Some(w) => {
        val r = f(w.pos)
        (w put r, Some(r))
      }
    }))

  def :=[A >: A2 <: A1, B >: B1 <: B2](b: => B)(implicit F: Functor[F]): PStateT[F, A, B] =
    %=(_ => b)

  def %==[A >: A2 <: A1, B >: B1 <: B2](f: B1 => B)(implicit F: Functor[F]): StateT[F, A, Unit] =
    StateT(a =>
      F.map(mod(f, a))((_, ())))

  def %%=[C, A >: A2 <: A1, B >: B1 <: B2](s: State[B, C])(implicit F: Functor[F]): PStateT[F, A, C] =
    StateT(a => F.map(run(a))(_ match {
      case None => (a, None)
      case Some(w) => {
        val r = s.run(w.pos): (B, C)
        (w put r._1, Some(r._2))
      }
    }))

  def >-[C, A >: A2 <: A1](f: B1 => C)(implicit F: Functor[F]): PStateT[F, A, C] =
    StateT(a => F.map(get(a))(x => (a, x map f)))

  def >>-[C, A >: A2 <: A1](f: B1 => StateT[F, A, C])(implicit F: Monad[F]): PStateT[F, A, C] =
    StateT(a => F.bind(get(a))(_ match {
      case None => F.point((a, None))
      case Some(w) =>
        F.map(f(w) apply a) {
          case (y, x) => (y, Some(x))
        }
    }))

  def ->>-[C, A >: A2 <: A1](f: => StateT[F, A, C])(implicit F: Monad[F]): PStateT[F, A, C] =
    >>-(_ => f)

  /** Partial Lenses can be composed */
  def compose[C1, C2](that: PLensFamilyT[F, C1, C2, A1, A2])(implicit FF: Monad[F]): PLensFamilyT[F, C1, C2, B1, B2] =
    plensFamilyO(c =>
      (that runO c).flatMap (x => {
        val (ac, a) = x.run
        runO(a) map (y => {
          val (ba, b) = y.run
          IndexedStore(ac compose ba, b)
        })
      }))

  /** alias for `compose` */
  def <=<[C1, C2](that: PLensFamilyT[F, C1, C2, A1, A2])(implicit FF: Monad[F]): PLensFamilyT[F, C1, C2, B1, B2] = compose(that)

  def andThen[C1, C2](that: PLensFamilyT[F, B1, B2, C1, C2])(implicit FF: Monad[F]): PLensFamilyT[F, A1, A2, C1, C2] =
    that compose this

  /** alias for `andThen` */
  def >=>[C1, C2](that: PLensFamilyT[F, B1, B2, C1, C2])(implicit FF: Monad[F]): PLensFamilyT[F, A1, A2, C1, C2] = andThen(that)

  /** Two partial lenses that view a value of the same type can be joined */
  def sum[C1, C2](that: => PLensFamilyT[F, C1, C2, B1, B2])(implicit F: Functor[F]): PLensFamilyT[F, A1 \/ C1, A2 \/ C2, B1, B2] =
    plensFamilyT{
      case -\/(a) =>
        F.map(run(a))(_ map (_ map (-\/(_))))
      case \/-(c) =>
        F.map(that run c)(_ map (_ map (\/-(_))))
    }

  /** Alias for `sum` */
  def |||[C1, C2](that: => PLensFamilyT[F, C1, C2, B1, B2])(implicit F: Functor[F]): PLensFamilyT[F, A1 \/ C1, A2 \/ C2, B1, B2] = sum(that)

  /** Two disjoint partial lenses can be paired */
  def product[C1, C2, D1, D2](that: PLensFamilyT[F, C1, C2, D1, D2])(implicit FF: Apply[F]): PLensFamilyT[F, (A1, C1), (A2, C2), (B1, D1), (B2, D2)] =
    plensFamilyT {
      case (a, c) =>
        FF.apply2(run(a), that run c)((x, y) => for {
          q <- x
          r <- y
        } yield q *** r)
    }

  /** alias for `product` */
  def ***[C1, C2, D1, D2](that: PLensFamilyT[F, C1, C2, D1, D2])(implicit FF: Apply[F]): PLensFamilyT[F, (A1, C1), (A2, C2), (B1, D1), (B2, D2)] = product(that)

}

object PLensFamilyT extends PLensTFunctions with PLensTInstances {
  def apply[F[+_], A1, A2, B1, B2](r: A1 => F[Option[IndexedStore[B1, B2, A2]]]): PLensFamilyT[F, A1, A2, B1, B2] =
    plensFamilyT(r)
}

trait PLensFamilyTFunctions extends PLensTInstances {

  import StoreT._
  import BijectionT._

  def plensFamilyT[F[+_], A1, A2, B1, B2](r: A1 => F[Option[IndexedStore[B1, B2, A2]]]): PLensFamilyT[F, A1, A2, B1, B2] = new PLensFamilyT[F, A1, A2, B1, B2] {
    def run(a: A1): F[Option[IndexedStore[B1, B2, A2]]] = r(a)
  }

  def plensFamilyO[F[+_], A1, A2, B1, B2](r: A1 => OptionT[F, IndexedStore[B1, B2, A2]]): PLensFamilyT[F, A1, A2, B1, B2] =
    plensFamilyT(a => r(a).run)

  def plensFamily[A1, A2, B1, B2](r: A1 => Option[IndexedStore[B1, B2, A2]]): PLensFamily[A1, A2, B1, B2] =
    plensFamilyT[Id, A1, A2, B1, B2](r)

  def plensFamilyf[A1, A2, B1, B2](r: PartialFunction[A1, IndexedStore[B1, B2, A2]]): PLensFamily[A1, A2, B1, B2] =
    plensFamily(r.lift)

  def plensFamilyp[F[+_], A1, A2, B1, B2](r: A1 => Option[IndexedStore[B1, B2, A2]])(implicit PF: Pointed[F]): PLensFamilyT[F, A1, A2, B1, B2] =
    plensFamilyT(a => PF.point(r(a)))

  def plensFamilypf[F[+_], A1, A2, B1, B2](r: PartialFunction[A1, IndexedStore[B1, B2, A2]])(implicit PF: Pointed[F]): PLensFamilyT[F, A1, A2, B1, B2] =
    plensFamilyp(r.lift)

  def plensFamilygT[F[+_], A1, A2, B1, B2](set: A1 => F[Option[B2 => A2]], get: A1 => F[Option[B1]])(implicit M: Bind[F]): PLensFamilyT[F, A1, A2, B1, B2] =
    plensFamilyT(a => M.apply2(set(a), get(a))((q, r) => for {
      w <- q
      x <- r
    } yield IndexedStore(w, x)))

  def plensFamilygO[F[+_], A1, A2, B1, B2](set: A1 => OptionT[F, B2 => A2], get: A1 => OptionT[F, B1])(implicit M: Bind[F]): PLensFamilyT[F, A1, A2, B1, B2] =
    plensFamilygT[F, A1, A2, B1, B2](a => set(a).run, a => get(a).run)

  def plensFamilyg[A1, A2, B1, B2](set: A1 => Option[B2 => A2], get: A1 => Option[B1]): PLensFamily[A1, A2, B1, B2] =
    plensFamilygT[Id, A1, A2, B1, B2](set, get)

  def plensFamilygf[A1, A2, B1, B2](set: PartialFunction[A1, B2 => A2], get: PartialFunction[A1, B1]): PLensFamily[A1, A2, B1, B2] =
    plensFamilyg(set.lift, get.lift)

  /** The identity partial lens family for a given pair of objects */
  def plensFamilyId[F[+_], A1, A2](implicit FF: Pointed[F]): PLensFamilyT[F, A1, A2, A1, A2] =
    LensFamilyT.lensFamilyId[F, A1, A2].partial

  /** A partial lens family that discards the choice of right or left from disjunction */
  def codiagPLensFamily[F[+_]: Pointed, A1, A2]: PLensFamilyT[F, A1 \/ A1, A2 \/ A2, A1, A2] =
    plensFamilyId[F, A1, A2] ||| plensFamilyId[F, A1, A2]

  /** The always-null partial lens family */
  def nilFamily[F[+_]: Pointed, A1, A2, B1, B2]: PLensFamilyT[F, A1, A2, B1, B2] =
    plensFamilyp(_ => None)

  def somePLensFamily[A1, A2]: PLensFamily[Option[A1], Option[A2], A1, A2] =
    plensFamily(_ map (z => Store(Some(_), z)))

  def leftPLensFamily[A1, A2, B]: PLensFamily[A1 \/ B, A2 \/ B, A1, A2] =
    plensFamily {
      case -\/(a) => Some(Store(-\/(_), a))
      case \/-(_) => None
    }

  def rightPLensFamily[A, B1, B2]: PLensFamily[A \/ B1, A \/ B2, B1, B2] =
    plensFamily {
      case \/-(b) => Some(Store(\/-(_), b))
      case -\/(_) => None
    }

  def tuple2PLensFamily[F[+_]: Functor, S1, S2, A, B](lens: PLensFamilyT[F, S1, S2, (A, B), (A, B)]):
  (PLensFamilyT[F, S1, S2, A], PLensFamilyT[F, S1, S2, B]) =
    PLensFamilyTUnzip[F, S1, S2].unzip(lens)

  def tuple3PLensFamily[F[+_]: Functor, S1, S2, A, B, C](lens: PLensFamilyT[F, S1, S2, (A, B, C), (A, B, C)]):
  (PLensFamilyT[F, S1, S2, A], PLensFamilyT[F, S1, S2, B], PLensFamilyT[F, S1, S2, C]) =
    PLensFamilyTUnzip[F, S1, S2].unzip3(lens.xmapbB(tuple3B))

  def tuple4PLensFamily[F[+_]: Functor, S1, S2, A, B, C, D](lens: PLensFamilyT[F, S1, S2, (A, B, C, D), (A, B, C, D)]):
  (PLensFamilyT[F, S1, S2, A, A], PLensFamilyT[F, S1, S2, B, B], PLensFamilyT[F, S1, S2, C, C], PLensFamilyT[F, S1, S2, D, D]) =
    PLensFamilyTUnzip[F, S1, S2].unzip4(lens.xmapbB(tuple4B))

  def tuple5PLensFamily[F[+_]: Functor, S1, S2, A, B, C, D, E](lens: PLensFamilyT[F, S1, S2, (A, B, C, D, E), (A, B, C, D, E)]):
  (PLensFamilyT[F, S1, S2, A, A], PLensFamilyT[F, S1, S2, B, B], PLensFamilyT[F, S1, S2, C, C], PLensFamilyT[F, S1, S2, D, D], PLensFamilyT[F, S1, S2, E, E]) =
    PLensFamilyTUnzip[F, S1, S2].unzip5(lens.xmapbB(tuple5B))

  def tuple6PLensFamily[F[+_]: Functor, S1, S2, A, B, C, D, E, H](lens: PLensFamilyT[F, S1, S2, (A, B, C, D, E, H), (A, B, C, D, E, H)]):
  (PLensFamilyT[F, S1, S2, A, A], PLensFamilyT[F, S1, S2, B, B], PLensFamilyT[F, S1, S2, C, C], PLensFamilyT[F, S1, S2, D, D], PLensFamilyT[F, S1, S2, E, E], PLensFamilyT[F, S1, S2, H, H]) =
    PLensFamilyTUnzip[F, S1, S2].unzip6(lens.xmapbB(tuple6B))

  def tuple7PLensFamily[F[+_]: Functor, S1, S2, A, B, C, D, E, H, I](lens: PLensFamilyT[F, S1, S2, (A, B, C, D, E, H, I), (A, B, C, D, E, H, I)]):
  (PLensFamilyT[F, S1, S2, A, A], PLensFamilyT[F, S1, S2, B, B], PLensFamilyT[F, S1, S2, C, C], PLensFamilyT[F, S1, S2, D, D], PLensFamilyT[F, S1, S2, E, E], PLensFamilyT[F, S1, S2, H, H], PLensFamilyT[F, S1, S2, I, I]) =
    PLensFamilyTUnzip[F, S1, S2].unzip7(lens.xmapbB(tuple7B))

  def eitherLensFamily[S1, S2, A1, A2, B1, B2](l: PLensFamily[S1, S2, A1 \/ B1, A2 \/ B2]): (PLensFamily[S1, S2, A1, A2], PLensFamily[S1, S2, B1, B2]) =
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
    ~LensFamilyT.factorLensFamily

  def distributePLens[A1, A2, B1, B2, C1, C2]: PLensFamily[(A1, B1 \/ C1), (A2, B2 \/ C2), ((A1, B1) \/ (A1, C1)), ((A2, B2) \/ (A2, C2))] =
    ~LensFamilyT.distributeLensFamily
}

trait PLensTFunctions extends PLensFamilyTFunctions with PLensTInstances {

  import StoreT._
  import BijectionT._

  def plensT[F[+_], A, B](r: A => F[Option[Store[B, A]]]): PLensT[F, A, B] = new PLensT[F, A, B] {
    def run(a: A): F[Option[Store[B, A]]] = r(a)
  }

  def plensO[F[+_], A, B](r: A => OptionT[F, Store[B, A]]): PLensT[F, A, B] =
    plensT(a => r(a).run)

  def plens[A, B](r: A => Option[Store[B, A]]): PLens[A, B] =
    plensT[Id, A, B](r)

  def plensf[A, B](r: PartialFunction[A, Store[B, A]]): PLens[A, B] =
    plens(r.lift)

  def plensp[F[+_], A, B](r: A => Option[Store[B, A]])(implicit PF: Pointed[F]): PLensT[F, A, B] =
    plensT(a => PF.point(r(a)))

  def plenspf[F[+_], A, B](r: PartialFunction[A, Store[B, A]])(implicit PF: Pointed[F]): PLensT[F, A, B] =
    plensp(r.lift)

  def plensgT[F[+_], A, B](set: A => F[Option[B => A]], get: A => F[Option[B]])(implicit M: Bind[F]): PLensT[F, A, B] =
    plensT(a => M.apply2(set(a), get(a))((q, r) => for {
      w <- q
      x <- r
    } yield Store(w, x)))

  def plensgO[F[+_], A, B](set: A => OptionT[F, B => A], get: A => OptionT[F, B])(implicit M: Bind[F]): PLensT[F, A, B] =
    plensgT[F, A, B](a => set(a).run, a => get(a).run)

  def plensg[A, B](set: A => Option[B => A], get: A => Option[B]): PLens[A, B] =
    plensgT[Id, A, B](set, get)

  def plensgf[A, B](set: PartialFunction[A, B => A], get: PartialFunction[A, B]): PLens[A, B] =
    plensg(set.lift, get.lift)

  /** The identity partial lens for a given object */
  def plensId[F[+_], A](implicit FF: Pointed[F]): PLensT[F, A, A] =
    LensT.lensId[F, A].partial

  /** The trivial partial lens that can retrieve Unit from anything */
  def trivialPLens[F[+_], A](implicit FF: Pointed[F]): PLensT[F, A, Unit] =
    LensT.trivialLens[F, A].partial

  /** A lens that discards the choice of right or left from disjunction */
  def codiagPLens[F[+_]: Pointed, A]: PLensT[F, A \/ A, A] =
    plensId[F, A] ||| plensId[F, A]

  /** The always-null partial lens */
  def nil[F[+_]: Pointed, A, B]: PLensT[F, A, B] =
    plensp(_ => None)

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

  def tuple2PLens[F[+_]: Functor, S, A, B](lens: PLensT[F, S, (A, B)]):
  (PLensT[F, S, A], PLensT[F, S, B]) =
    PLensTUnzip[F, S].unzip(lens)

  def tuple3PLens[F[+_]: Functor, S, A, B, C](lens: PLensT[F, S, (A, B, C)]):
  (PLensT[F, S, A], PLensT[F, S, B], PLensT[F, S, C]) =
    PLensTUnzip[F, S].unzip3(lens.xmapbB(tuple3B))

  def tuple4PLens[F[+_]: Functor, S, A, B, C, D](lens: PLensT[F, S, (A, B, C, D)]):
  (PLensT[F, S, A], PLensT[F, S, B], PLensT[F, S, C], PLensT[F, S, D]) =
    PLensTUnzip[F, S].unzip4(lens.xmapbB(tuple4B))

  def tuple5PLens[F[+_]: Functor, S, A, B, C, D, E](lens: PLensT[F, S, (A, B, C, D, E)]):
  (PLensT[F, S, A], PLensT[F, S, B], PLensT[F, S, C], PLensT[F, S, D], PLensT[F, S, E]) =
    PLensTUnzip[F, S].unzip5(lens.xmapbB(tuple5B))

  def tuple6PLens[F[+_]: Functor, S, A, B, C, D, E, H](lens: PLensT[F, S, (A, B, C, D, E, H)]):
  (PLensT[F, S, A], PLensT[F, S, B], PLensT[F, S, C], PLensT[F, S, D], PLensT[F, S, E], PLensT[F, S, H]) =
    PLensTUnzip[F, S].unzip6(lens.xmapbB(tuple6B))

  def tuple7PLens[F[+_]: Functor, S, A, B, C, D, E, H, I](lens: PLensT[F, S, (A, B, C, D, E, H, I)]):
  (PLensT[F, S, A], PLensT[F, S, B], PLensT[F, S, C], PLensT[F, S, D], PLensT[F, S, E], PLensT[F, S, H], PLensT[F, S, I]) =
    PLensTUnzip[F, S].unzip7(lens.xmapbB(tuple7B))

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

  import LensT.mapVLens

  def mapVPLens[K, V](k: K): Map[K, V] @?> V =
    somePLens compose ~mapVLens[K, V](k)

  def factorPLens[A, B, C]: ((A, B) \/ (A, C)) @?> (A, B \/ C) =
    ~LensT.factorLens

  def distributePLens[A, B, C]: (A, B \/ C) @?> ((A, B) \/ (A, C)) =
    ~LensT.distributeLens

  import util.parsing.json._

  def scalaJSONObjectPLens[A]: JSONType @?> Map[String, Any] =
    plens {
      case JSONObject(m) => Some(Store(JSONObject(_), m))
      case _             => None
    }

  def scalaJSONArrayPLens[A]: JSONType @?> List[Any] =
    plens {
      case JSONArray(a) => Some(Store(JSONArray(_), a))
      case _            => None
    }
}

trait PLensTInstance0 {
  implicit def plensTArrId[F[+_]](implicit F0: Pointed[F]) = new PLensTArrId[F] {
    implicit def F = F0
  }
}

trait PLensTInstances extends PLensTInstance0 {
  import PLensFamilyT._

  implicit def plensTCategory[F[+_]](implicit F0: Monad[F]) = new PLensTCategory[F] {
    implicit def F = F0
  }

  /** Partial Lenses may be used implicitly as State monadic actions that get the potentially viewed portion of the state */
  implicit def PLensFamilyState[F[+_], A, B](plens: PLensFamilyT[F, A, _, B, _])(implicit F: Functor[F]): PStateT[F, A, B] =
    plens.st

  implicit def PLensTFamilyUnzip[F[+_], S, R](implicit F: Functor[F]): Unzip[({type λ[α] = PLensFamilyT[F, S, R, α, α]})#λ] =
    new Unzip[({type λ[α] = PLensFamilyT[F, S, R, α, α]})#λ] {
      def unzip[A, B](a: PLensFamilyT[F, S, R, (A, B), (A, B)]) =
        (
          PLensT(x => F.map(a run x)(_ map (c => {
            val (p, q) = c.pos
            Store(a => c.put((a, q)): R, p)
          })))
          , PLensT(x => F.map(a run x)(_ map (c => {
          val (p, q) = c.pos
          Store(a => c.put((p, a)): R, q)
        })))
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

private[scalaz] trait PLensTArrId[F[+_]] extends ArrId[({type λ[α, β] = PLensT[F, α, β]})#λ] {
  implicit def F: Pointed[F]
  def id[A]: PLensT[F, A, A] = PLensT.plensId
}

private[scalaz] trait PLensTCategory[F[+_]]
  extends Choice[({type λ[α, β] = PLensT[F, α, β]})#λ]
  with Split[({type λ[α, β] = PLensT[F, α, β]})#λ]
  with PLensTArrId[F] {

  implicit def F: Monad[F]

  def compose[A, B, C](bc: PLensT[F, B, C], ab: PLensT[F, A, B]): PLensT[F, A, C] = ab >=> bc

  def choice[A, B, C](f: => PLensT[F, A, C], g: => PLensT[F, B, C]): PLensT[F, A \/ B, C] =
    PLensT.plensT[F, A \/ B, C] {
      case -\/(a) =>
        F.map(f run a)(_ map (_ map (-\/(_))))
      case \/-(b) =>
        F.map(g run b)(_ map (_ map (\/-(_))))
    }

  def split[A, B, C, D](f: PLensT[F, A, B], g: PLensT[F, C, D]): PLensT[F, (A,  C), (B, D)] =
    f *** g
}
