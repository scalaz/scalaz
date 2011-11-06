package scalaz

sealed trait LazyOption[A] {

  import LazyOption._
  import LazyEither._
//  import newtypes.{FirstLazyOption, LastLazyOption}

  def fold[X](some: (=> A) => X, none: => X): X =
    this match {
      case LazySome(z) => some(z())
      case LazyNone()  => none
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

  def ap[B](f: LazyOption[A => B]): LazyOption[B] =
    f flatMap (k => map(k apply _))

  def traverse[G[_] : Applicative, B](f: (=> A) => G[B]): G[LazyOption[B]] =
    fold(
      some = x => Applicative[G].map(f(x))(b => LazyOption.lazySome(b)),
      none = Applicative[G].point(LazyOption.lazyNone[B])
    )

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    fold(
      some = a => f(a, z),
      none = z
    )
}

private case class LazySome[A](a: () => A) extends LazyOption[A]

private case class LazyNone[A]() extends LazyOption[A]

object LazyOption extends LazyOptionFunctions with LazyOptionInstances

trait LazyOptionInstances {
  import LazyOption._

  implicit object lazyOptionInstance extends Traverse[LazyOption] with MonadPlus[LazyOption] {
    def traverseImpl[G[_]: Applicative, A, B](fa: LazyOption[A])(f: (A) => G[B]): G[LazyOption[B]] =  fa traverse (a => f(a))
    def foldRight[A, B](fa: LazyOption[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
    override def ap[A, B](fa: LazyOption[A])(f: LazyOption[A => B]): LazyOption[B] = fa ap f
    def plus[A](a: LazyOption[A], b: => LazyOption[A]): LazyOption[A] = a orElse b
    def bind[A, B](fa: LazyOption[A])(f: (A) => LazyOption[B]): LazyOption[B] = fa flatMap (a => f(a))
    def point[A](a: => A): LazyOption[A] = lazySome(a)
    def empty[A]: LazyOption[A] = lazyNone
  }
  /* TODO
implicit def LazyOptionShow[A: Show]: Show[LazyOption[A]] =
  Show[A].shows(_ map (implicitly[Show[A]].shows(_)) fold ("~Some(" + _ + ")", "~None"))

implicit def LazyOptionEqual[A: Equal]: Equal[LazyOption[A]] =
  Equal.equalBy(_.toOption)

implicit def LazyOptionOrder[A: Order]: Order[LazyOption[A]] =
  Order.orderBy(_.toOption)*/
}

trait LazyOptionFunctions {
  def lazySome[A]: (=> A) => LazyOption[A] =
    a => LazySome(() => a)

  def lazyNone[A]: LazyOption[A] =
    LazyNone()

  /**
   * Returns the given argument in `lazySome` if this is `true`, `lazyNone` otherwise.
   */
  def condLazyOption[A](value: Boolean, a: => A): LazyOption[A] = if (value) lazySome(a) else lazyNone
}
