package scalaz

import CoStateT._

sealed trait PLens[A, B] {
  import PLens._
  import CoStateT._

  def run(a: A): Option[CoState[B, A]]

  def compose[C](that: PLens[C, A]): PLens[C, B] =
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

  def set(b: B, a: A): A =
    run(a) match {
      case None => a
      case Some(w) => w put b
    }

  def mod(f: B => B, a: A): A =
    run(a) match {
      case None => a
      case Some(w) => w.puts(f)
    }
}

object PLens extends PLensFunctions with PLensInstances {
  def apply[A, B](r: A => Option[CoState[B, A]]): PLens[A, B] =
    plens(r)
}

trait PLensInstances {
  import PLens._

  implicit def plensCategory: Category[PLens] = new Category[PLens] {
    def compose[A, B, C](f: PLens[B, C], g: PLens[A, B]): PLens[A, C] =
      f compose g
    def id[A]: PLens[A, A] =
      plensId[A]
  }
}

trait PLensFunctions {

  import CoStateT._

  /** The eye-patch operator, an alias for `PLens` */
  type @-?[A, B] =
  PLens[A, B]

  def plens[A, B](r: A => Option[CoState[B, A]]): PLens[A, B] = new PLens[A, B] {
    def run(a: A) = r(a)
  }

  /** The identity partial lens for a given object */
  def plensId[A]: PLens[A, A] =
    implicitly[Category[Lens]].id.partial

  /** The always-null partial lens */
  def nil[A, B]: PLens[A, B] =
    plens(_ => None)
}