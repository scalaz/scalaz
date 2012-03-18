package scalaz

import CoStateT._

sealed trait PLens[A, B] {
  import PLens._
  import CoStateT._

  def run(a: A): Option[A |--> B]

  def get(a: A): Option[B] =
    run(a) map (_.pos)

  def getK: A =?> B =
    Kleisli(get(_))

  def setK: A =?> (B => A) =
    Kleisli(set(_))

  /** If the PartialLens is null, then return the given default value. */
  def getOr(a: A, b: => B): B =
    get(a) getOrElse b

  /** (Monadic) If the PartialLens is null, then return the given default value. Only the first effect is run. */
  def getOrM[F[_]](a: F[A], b: => F[B])(implicit M: Monad[F]): F[B] =
    M.bind(a)(r => get(r) match {
      case None => b
      case Some(c) => M.point(c)
    })

  /** If the PartialLens is null, then return the target object, otherwise run the function on its projection. */
  def as(f: B => A, a: A): A =
    get(a) match {
      case None => a
      case Some(w) => f(w)
    }

  def is(a: A): Boolean =
    get(a).isDefined

  def isNot(a: A): Boolean =
    !is(a)

  def exists(p: B => Boolean, a: A): Boolean =
    get(a) exists p

  def forall(p: B => Boolean, a: A): Boolean =
    get(a) forall p

  def trySet(a: A): Option[B => A] =
    run(a) map (c => c.put(_))

  def set(a: A): Option[B => A] =
    run(a) map (w => w.put(_))

  def setI(a: A, b: B): A =
    run(a) match {
      case None => a
      case Some(w) => w put b
    }

  def mod(f: B => B, a: A): A =
    run(a) match {
      case None => a
      case Some(w) => w.puts(f)
    }

  def =>=(f: B => B): A => A =
    mod(f, _)

  def modE(f: Endo[B]): Endo[A] =
    Endo(=>=(f.run))

  /** Modify the value viewed through the lens, a functor full of results */
  def modf[F[_]](f: B => F[B], a: A)(implicit F: Pointed[F]): F[A] =
    run(a) match {
      case None => F.point(a)
      case Some(w) => F.map(f(w.pos))(w.set)
    }

  def st: PState[A, B] =
    State.init map (get(_))

  def :=(b: => B): PState[A, B] =
    %=(_ => b)

  def %=(f: B => B): PState[A, B] =
    State(a => run(a) match {
      case None => (None, a)
      case Some(w) => {
        val r = f(w.pos)
        (Some(r), w put r)
      }
    })

  def %==(f: B => B): State[A, Unit] =
    State(a => ((), mod(f, a)))

  def %%=[C](s: State[B, C]): PState[A, C] =
    State(a => run(a) match {
      case None => (None, a)
      case Some(w) => {
        val r = s.run(w.pos): (C, B)
        (Some(r._1), w put r._2)
      }
    })

  def >-[C](f: B => C): PState[A, C] =
    State(a => (get(a) map f, a))

  def >>-[C](f: B => State[A, C]): PState[A, C] =
    State(a =>
      get(a) map f match {
        case None => (None, a)
        case Some(w) => (Some(w.apply(a)._1), a)
      })

  def ->>-[C](f: => State[A, C]): PState[A, C] =
    >>-(_ => f)

  def compose[C](that: C @-? A): C @-? B =
    plens(a => for {
      s <- that run a
      t <- run(s.pos)
    } yield coState(x => s.put(t.put(x)), t.pos))

  /** alias for `compose` */
  def >=>[C](that: C @-? A): C @-? B = compose(that)

  def andThen[C](that: B @-? C): A @-? C =
    that compose this

  /** alias for `andThen` */
  def <=<[C](that: B @-? C): A @-? C = andThen(that)

  /** Two partial lenses that view a value of the same type can be joined */
  def sum[C](that: => C @-? B): Either[A, C] @-? B =
    plens {
      case Left(a) =>
        run(a) map (x => coState(w => Left(x put w), x.pos))
      case Right(b) =>
        that.run(b) map (y => coState(w => Right(y put w), y.pos))
    }

  /** Alias for `sum` */
  def |||[C](that: => C @-? B): Either[A, C] @-? B= sum(that)

  /** Two disjoint partial lenses can be paired */
  def product[C, D](that: C @-? D): (A, C) @-? (B, D) =
    plens {
      case (a, c) => for {
        x <- run(a)
        y <- that run c
      } yield coState(bd => (x put bd._1, y put bd._2), (x.pos, y.pos))
    }

  /** alias for `product` */
  def ***[C, D](that: C @-? D): (A, C) @-? (B, D) = product(that)
}

object PLens extends PLensFunctions with PLensInstances {
  def apply[A, B](r: A => Option[A |--> B]): A @-? B =
    plens(r)
}

trait PLensInstances {
  import PLens._

  implicit def plensCategory: Category[PLens] with Choice[PLens] with Split[PLens] with Codiagonal[PLens] = new Category[PLens] with Choice[PLens] with Split[PLens] with Codiagonal[PLens] {
    def compose[A, B, C](f: B @-? C, g: A @-? B): A @-? C =
      f compose g
    def id[A]: A @-? A =
      plensId[A]
    def choice[A, B, C](f: => A @-? C, g: => B @-? C): Either[A,  B] @-? C =
      plens {
        case Left(a) =>
          f.run(a) map (x => coState(w => Left(x put w), x.pos))
        case Right(b) =>
          g.run(b) map (y => coState(w => Right(y put w), y.pos))
      }
    def split[A, B, C, D](f: A @-? B, g: C @-? D): (A,  C) @-? (B, D) =
      f *** g
    def codiagonal[A]: Either[A,  A] @-? A =
      codiagPLens

  }

  /** Partial lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def PLensState[A, B](lens: A @-? B): PState[A, B] =
    lens.st

  implicit def eitherLens[S, A, B](l: S @-? Either[A, B]): (S @-? A, S @-? B) =
    (
      leftPLens compose l
    , rightPLens compose l
    )

  /** Allow the illusion of imperative updates to numbers viewed through a partial lens */
  case class NumericPLens[S, N: Numeric](lens: PLens[S, N], num: Numeric[N]) {
    def +=(that: N): PState[S, N] =
      lens %= (num.minus(_, that))

    def -=(that: N): PState[S, N] =
      lens %= (num.minus(_, that))

    def *=(that: N): PState[S, N] =
      lens %= (num.times(_, that))
  }

  implicit def numericPLens[S, N: Numeric](lens: S @-? N) =
    NumericPLens[S, N](lens, implicitly[Numeric[N]])

  /** Allow the illusion of imperative updates to numbers viewed through a partial lens */
  case class FractionalPLens[S, F](lens: S @-? F, frac: Fractional[F]) {
    def /=(that: F): PState[S, F] =
      lens %= (frac.div(_, that))
  }

  implicit def fractionalPLens[S, F: Fractional](lens: S @-? F) =
    FractionalPLens[S, F](lens, implicitly[Fractional[F]])

  /** Allow the illusion of imperative updates to numbers viewed through a partial lens */
  case class IntegralPLens[S, I](lens: S @-? I, ig: Integral[I]) {
    def %=(that: I): PState[S, I] =
      lens %= (ig.quot(_, that))
  }

  implicit def integralPLens[S, I: Integral](lens: S @-? I) =
    IntegralPLens[S, I](lens, implicitly[Integral[I]])
}

trait PLensFunctions {

  /** The eye-patch operator, an alias for `PLens` */
  type @-?[A, B] =
  PLens[A, B]

  type PState[A, B] =
  State[A, Option[B]]

  def plens[A, B](r: A => Option[A |--> B]): A @-? B = new PLens[A, B] {
    def run(a: A) = r(a)
  }

  def plensG[A, B](get: A => Option[B], set: A => Option[B => A]): A @-? B =
    plens(a => for {
      g <- get(a)
      s <- set(a)
    } yield coState(s, g))

  /** The identity partial lens for a given object */
  def plensId[A]: A @-? A =
    implicitly[Category[Lens]].id.partial

  /** The trivial partial lens that can retrieve Unit from anything */
  def trivialPLens[A]: A @-? Unit =
    plensG[A, Unit](_ => Some(()), a => Some(_ => a))

  /** A lens that discards the choice of Right or Left from Either */
  def codiagPLens[A]: PLens[Either[A, A], A] =
    plensId[A] ||| plensId[A]

  /** The always-null partial lens */
  def nil[A, B]: A @-? B =
    plens(_ => None)

  def optionPLens[A]: Option[A] @-? A =
    plens(_ map (z => coState(Some(_), z)))

  def leftPLens[A, B]: Either[A, B] @-? A =
    plens {
      case Left(a) => Some(coState(Left(_), a))
      case Right(_) => None
    }

  def rightPLens[A, B]: Either[A, B] @-? B =
    plens {
      case Right(b) => Some(coState(Right(_), b))
      case Left(_) => None
    }

  import LazyOption._

  def lazyOptionPLens[A]: LazyOption[A] @-? A =
    plens(_.fold(z => Some(coState(lazySome(_), z)), None))

  import LazyEither._

  def lazyLeftPLens[A, B]: LazyEither[A, B] @-? A =
    plens(_.fold(a => Some(coState(lazyLeft(_), a)), _ => None))

  def lazyRightPLens[A, B]: LazyEither[A, B] @-? B =
    plens(_.fold(_ => None, b => Some(coState(lazyRight(_), b))))

  def listHeadPLens[A]: List[A] @-? A =
    plens {
      case Nil => None
      case h :: t => Some(coState(_ :: t, h))
    }

  def listTailPLens[A]: List[A] @-? List[A] =
    plens {
      case Nil => None
      case h :: t => Some(coState(h :: _, t))
    }

  def listNthPLens[A](n: Int): List[A] @-? A =
    if(n < 0)
      nil
    else if(n == 0)
      listHeadPLens
    else
      listNthPLens(n - 1) compose listTailPLens

  import Stream._

  def streamHeadPLens[A]: Stream[A] @-? A =
    plens {
      case Empty => None
      case h #:: t => Some(coState(_ #:: t, h))
    }

  def streamTailPLens[A]: Stream[A] @-? Stream[A] =
    plens {
      case Empty => None
      case h #:: t => Some(coState(h #:: _, t))
    }

  def streamNthPLens[A](n: Int): Stream[A] @-? A =
    if(n < 0)
      nil
    else if(n == 0)
      streamHeadPLens
    else
      streamNthPLens(n - 1) compose streamTailPLens

  def ephemeralStreamHeadPLens[A]: EphemeralStream[A] @-? A =
    plens(s =>
      if(s.isEmpty)
        None
      else
        Some(coState(EphemeralStream.cons(_, s.tail()), s.head()))
    )

  def ephemeralStreamTailPLens[A]: EphemeralStream[A] @-? EphemeralStream[A] =
    plens(s =>
      if(s.isEmpty)
        None
      else
        Some(coState(EphemeralStream.cons(s.head(), _), s.tail()))
    )

  def ephemeralStreamNthPLens[A](n: Int): EphemeralStream[A] @-? A =
    if(n < 0)
      nil
    else if(n == 0)
      ephemeralStreamHeadPLens
    else
      ephemeralStreamNthPLens(n - 1) compose ephemeralStreamTailPLens

  import util.parsing.json._

  def scalaJSONObjectPLens[A]: JSONType @-? Map[String, Any] =
    plens {
      case JSONObject(m) => Some(coState(JSONObject(_), m))
      case _             => None
    }

  def scalaJSONArrayPLens[A]: JSONType @-? List[Any] =
    plens {
      case JSONArray(a) => Some(coState(JSONArray(_), a))
      case _             => None
    }
}
