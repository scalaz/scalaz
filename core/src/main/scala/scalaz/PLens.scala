package scalaz

import CoStateT._

sealed trait PLens[A, B] {
  import PLens._
  import CoStateT._

  def run(a: A): Option[A |--> B]

  def compose[C](that: C @-? A): C @-? B =
    plens(a => for {
      s <- that run a
      t <- run(s.pos)
    } yield coState(x => s.put(t.put(x)), t.pos))

  def get(a: A): Option[B] =
    run(a) map (_.pos)

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

  def set(a: A, b: B): A =
    run(a) match {
      case None => a
      case Some(w) => w put b
    }

  def mod(f: B => B, a: A): A =
    run(a) match {
      case None => a
      case Some(w) => w.puts(f)
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

}

object PLens extends PLensFunctions with PLensInstances {
  def apply[A, B](r: A => Option[A |--> B]): A @-? B =
    plens(r)
}

trait PLensInstances {
  import PLens._

  implicit def plensCategory: Category[PLens] = new Category[PLens] {
    def compose[A, B, C](f: B @-? C, g: A @-? B): A @-? C =
      f compose g
    def id[A]: A @-? A =
      plensId[A]
  }

  /** Partial lenses may be used implicitly as State monadic actions that get the viewed portion of the state */
  implicit def PLensState[A, B](lens: A @-? B): PState[A, B] =
    lens.st

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

  /** The identity partial lens for a given object */
  def plensId[A]: A @-? A =
    implicitly[Category[Lens]].id.partial

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

  def headPLens[A]: List[A] @-? A =
    plens {
      case Nil => None
      case h::t => Some(coState(_::t, h))
    }

  def tailPLens[A]: List[A] @-? List[A] =
    plens {
      case Nil => None
      case h::t => Some(coState(h::_, t))
    }
}