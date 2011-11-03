package scalaz

sealed trait LazyEither[A, B] {

  import LazyOption._
  import LazyEither._

  def fold[X](left: (=> A) => X, right: (=> B) => X): X =
    this match {
      case LazyLeft(a)  => left(a())
      case LazyRight(b) => right(b())
    }

  def ?[X](left: => X, right: => X): X =
    fold(_ => left, _ => right)

  def isLeft =
    fold(_ => true, _ => false)

  def isRight =
    !isLeft

  def swap: LazyEither[B, A] =
    fold(lazyRight(_), lazyLeft(_))

  def toEither: Either[A, B] =
    fold(Left(_), Right(_))

  def getOrElse(default: => B): B =
    fold(_ => default, z => z)

  def exists(f: (=> B) => Boolean): Boolean =
    fold(_ => false, f)

  def forall(f: (=> B) => Boolean): Boolean =
    fold(_ => true, f)

  def orElse(x: => LazyEither[A, B]): LazyEither[A, B] =
    ?(x, this)

  def toLazyOption: LazyOption[B] =
    fold(_ => lazyNone, lazySome(_))

  def toOption: Option[B] =
    fold(_ => None, Some(_))

  def toList: List[B] =
    fold(_ => Nil, List(_))

  def toStream: Stream[B] =
    fold(_ => Stream(), Stream(_))

  def map[C](f: (=> B) => C): LazyEither[A, C] =
    fold(lazyLeft(_), b => lazyRight(f(b)))

  def foreach(f: (=> B) => Unit): Unit =
    fold(_ => (), f)

  def flatMap[C](f: (=> B) => LazyEither[A, C]): LazyEither[A, C] =
    fold(lazyLeft(_), f)

  def left = new LazyLeftProjection[A, B]() {
    val e = LazyEither.this
  }

}

private case class LazyLeft[A, B](a: () => A) extends LazyEither[A, B]

private case class LazyRight[A, B](b: () => B) extends LazyEither[A, B]

object LazyEither extends LazyEitherFunctions with LazyEitherInstances {

  sealed trait LazyLeftProjection[A, B] {
    def e: LazyEither[A, B]

    import LazyOption._

    def getOrElse(default: => A): A =
      e.fold(z => z, _ => default)

    def exists(f: (=> A) => Boolean): Boolean =
      e.fold(f, _ => false)

    def forall(f: (=> A) => Boolean): Boolean =
      e.fold(f, _ => true)

    def orElse(x: => LazyEither[A, B]): LazyEither[A, B] =
      e.?(e, x)

    def toLazyOption: LazyOption[A] =
      e.fold(lazySome(_), _ => lazyNone)

    def toOption: Option[A] =
      e.fold(Some(_), _ => None)

    def toList: List[A] =
      e.fold(List(_), _ => Nil)

    def toStream: Stream[A] =
      e.fold(Stream(_), _ => Stream())

    def map[C](f: (=> A) => C): LazyEither[C, B] =
      e.fold(a => lazyLeft(f(a)), lazyRight(_))

    def foreach(f: (=> A) => Unit): Unit =
      e.fold(f, _ => ())

    def flatMap[C](f: (=> A) => LazyEither[C, B]): LazyEither[C, B] =
      e.fold(f, lazyRight(_))
  }

}

trait LazyEitherInstances {
  // TODO

  implicit def lazyEitherBiFunctor: BiFunctor[LazyEither] = new BiFunctor[LazyEither] {
    def bimap[A, B, C, D](fab: LazyEither[A, B])(f: A => C, g: B => D) =
      fab.map(x => g(x)).left.map(x => f(x))
  }
}

trait LazyEitherFunctions {


  /**
   * Returns the first argument in `LazyLeft` if `value` is `true`, otherwise the second argument in
   * `LazyRight`
   */
  def condLazyEither[A, B](cond: Boolean)(ifTrue: => A, ifFalse: => B): LazyEither[A, B] = if (cond) lazyLeft(ifTrue) else lazyRight(ifFalse)

  trait LazyLeftConstruct[B] {
    def apply[A](a: => A): LazyEither[A, B]
  }

  def lazyLeft[B]: LazyLeftConstruct[B] = new LazyLeftConstruct[B] {
    def apply[A](a: => A) = LazyLeft(() => a)
  }

  trait LazyRightConstruct[A] {
    def apply[B](b: => B): LazyEither[A, B]
  }

  def lazyRight[A]: LazyRightConstruct[A] = new LazyRightConstruct[A] {
    def apply[B](b: => B) = LazyRight(() => b)
  }
}
