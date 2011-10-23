package scalaz

sealed trait LazyOption[A] {

  import LazyOption._
  import LazyEither._
//  import newtypes.{FirstLazyOption, LastLazyOption}

  def fold[X](some: (=> A) => X, none: => X): X =
    this match {
      case LazySome(z) => some(z())
      case LazyNone() => none
    }

  def ?[X](some: => X, none: => X): X =
    fold(_ => some, none)

  def isDefined =
    fold(_ => false, true)

  def isEmpty =
    !isDefined

  def getOrElse(default: => A): A =
    fold(a => a, default)

  def exists(f: (=> A) => Boolean): Boolean =
    fold(f, false)

  def forall(f: (=> A) => Boolean): Boolean =
    fold(f, true)

  def toOption: Option[A] =
    fold(a => Some(a), None)

  def toLazyRight[X](left: => X): LazyEither[X, A] =
    fold(lazyRight(_), lazyLeft(left))

  def toLazyLeft[X](right: => X): LazyEither[A, X] =
    fold(lazyLeft(_), lazyRight(right))

  def toRight[X](left: => X): Either[X, A] =
    fold(Right(_), Left(left))

  def toLeft[X](right: => X): Either[A, X] =
    fold(Left(_), Right(right))

  def toList: List[A] =
    fold(List(_), Nil)

  def orElse(a: => LazyOption[A]): LazyOption[A] =
    fold(_ => this, a)

/* TODO
  def first: FirstLazyOption[A] =
    this.*-->[FirstLazyOption[A]]

  def last: LastLazyOption[A] =
    this.*-->[LastLazyOption[A]]
*/

  def map[B](f: (=> A) => B): LazyOption[B] =
    fold(a => lazySome(f(a)), lazyNone)

  def foreach(f: (=> A) => Unit): Unit =
    fold(f, ())

  def filter(f: (=> A) => Boolean): LazyOption[A] =
    fold(a => if (f(a)) this else lazyNone, lazyNone)

  def flatMap[B](f: (=> A) => LazyOption[B]): LazyOption[B] =
    fold(f, lazyNone)
}

private case class LazySome[A](a: () => A) extends LazyOption[A]

private case class LazyNone[A]() extends LazyOption[A]

object LazyOption extends LazyOptions

trait LazyOptions {
  def lazySome[A]: (=> A) => LazyOption[A] =
    a => LazySome(() => a)

  def lazyNone[A]: LazyOption[A] =
    LazyNone()

  /* TODO
  implicit def LazyOptionShow[A: Show]: Show[LazyOption[A]] =
    Show[A].shows(_ map (implicitly[Show[A]].shows(_)) fold ("~Some(" + _ + ")", "~None"))

  implicit def LazyOptionEqual[A: Equal]: Equal[LazyOption[A]] =
    Equal.equalBy(_.toOption)

  implicit def LazyOptionOrder[A: Order]: Order[LazyOption[A]] =
    Order.orderBy(_.toOption)*/
}
