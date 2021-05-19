package scalaz

/** [[scala.Either]], but with a value by name. */
sealed abstract class LazyEither[+A, +B] {

  import LazyOption._
  import LazyEither._

  def fold[X](left: (=> A) => X, right: (=> B) => X): X =
    this match {
      case LazyLeft(a)  => left(a())
      case LazyRight(b) => right(b())
    }

  /** Catamorphism of the constructor chosen. */
  def ?[X](left: => X, right: => X): X =
    fold(_ => left, _ => right)

  def isLeft: Boolean =
    fold(_ => true, _ => false)

  def isRight: Boolean =
    !isLeft

  def swap: LazyEither[B, A] =
    fold(lazyRight(_), lazyLeft(_))

  def toEither: Either[A, B] =
    fold(Left(_), Right(_))

  def disjunction: (A \/ B) =
    fold(-\/(_), \/-(_))

  def getOrElse[BB >: B](default: => BB): BB =
    fold(_ => default, z => z)

  def exists(f: (=> B) => Boolean): Boolean =
    fold(_ => false, f)

  def forall(f: (=> B) => Boolean): Boolean =
    fold(_ => true, f)

  def orElse[AA >: A, BB >: B](x: => LazyEither[AA, BB]): LazyEither[AA, BB] =
    ?(x, this)

  def toLazyOption: LazyOption[B] =
    fold(_ => lazyNone, lazySome(_))

  def toOption: Option[B] =
    fold(_ => None, Some(_))

  def toMaybe[BB >: B]: Maybe[BB] =
    fold(_ => Maybe.empty, Maybe.just(_))

  def toList: List[B] =
    fold(_ => Nil, _ :: Nil)

  def toStream: Stream[B] =
    fold(_ => Stream(), Stream(_))

  def map[C](f: (=> B) => C): LazyEither[A, C] =
    fold(lazyLeft(_), b => lazyRight(f(b)))

  def bimap[C, D](f: (=> A) => C, g: (=> B) => D): LazyEither[C, D] =
    fold(a => lazyLeft(f(a)), b => lazyRight(g(b)))

  /** Run the given function on the left value. */
  def leftMap[C](f: (=> A) => C): LazyEither[C, B] =
    fold(a => lazyLeft(f(a)), lazyRight(_))

  def foreach(f: (=> B) => Unit): Unit =
    fold(_ => (), f)

  def flatMap[AA >: A, C](f: (=> B) => LazyEither[AA, C]): LazyEither[AA, C] =
    fold(lazyLeft(_), f)

  def traverse[G[_]: Applicative, AA >: A, C](f: B => G[C]): G[LazyEither[AA, C]] =
    fold(
      left = x => Applicative[G].point(LazyEither.lazyLeft[C](x)),
      right = x => Applicative[G].map(f(x))(c => LazyEither.lazyRight[A](c))
    )

  def foldRight[Z](z: => Z)(f: (B, => Z) => Z): Z =
    fold(left = _ => z, right = a => f(a, z))

  def ap[AA >: A, C](f: => LazyEither[AA, B => C]): LazyEither[AA, C] =
    f flatMap (k => map(k apply _))

  def left: LeftProjection[A, B] =
    new LeftProjection[A, B](this)

}

private case class LazyLeft[A, B](a: () => A) extends LazyEither[A, B]

private case class LazyRight[A, B](b: () => B) extends LazyEither[A, B]

object LazyEither extends LazyEitherInstances {

  /**
   * Returns the first argument in `LazyLeft` if `value` is `true`, otherwise the second argument in
   * `LazyRight`
   */
  def condLazyEither[A, B](cond: Boolean)(ifTrue: => A, ifFalse: => B): LazyEither[A, B] = if (cond) lazyLeft(ifTrue) else lazyRight(ifFalse)

  sealed abstract class LazyLeftConstruct[B] {
    def apply[A](a: => A): LazyEither[A, B]
  }

  def lazyLeft[B]: LazyLeftConstruct[B] = new LazyLeftConstruct[B] {
    def apply[A](a: => A) = LazyLeft(() => a)
  }

  sealed abstract class LazyRightConstruct[A] {
    def apply[B](b: => B): LazyEither[A, B]
  }

  def lazyRight[A]: LazyRightConstruct[A] = new LazyRightConstruct[A] {
    def apply[B](b: => B) = LazyRight(() => b)
  }

  final case class LeftProjection[+A, +B](e: LazyEither[A, B]) {
    import LazyOption._

    def getOrElse[AA >: A](default: => AA): AA =
      e.fold(z => z, _ => default)

    def exists(f: (=> A) => Boolean): Boolean =
      e.fold(f, _ => false)

    def forall(f: (=> A) => Boolean): Boolean =
      e.fold(f, _ => true)

    def orElse[AA >: A, BB >: B](x: => LazyEither[AA, BB]): LazyEither[AA, BB] =
      e.?(e, x)

    def toLazyOption: LazyOption[A] =
      e.fold(lazySome(_), _ => lazyNone)

    def toOption: Option[A] =
      e.fold(Some(_), _ => None)

    def toList: List[A] =
      e.fold(_ :: Nil, _ => Nil)

    def toStream: Stream[A] =
      e.fold(Stream(_), _ => Stream())

    def map[C](f: (=> A) => C): LazyEither[C, B] =
      e.fold(a => lazyLeft(f(a)), lazyRight(_))

    def foreach(f: (=> A) => Unit): Unit =
      e.fold(f, _ => ())

    def flatMap[BB >: B, C](f: (=> A) => LazyEither[C, BB]): LazyEither[C, BB] =
      e.fold(f, lazyRight(_))
  }

}

// TODO more instances
sealed abstract class LazyEitherInstances {
  implicit def lazyEitherInstance[E]: Traverse[LazyEither[E, *]] with Monad[LazyEither[E, *]] with BindRec[LazyEither[E, *]] with Cozip[LazyEither[E, *]] with Optional[LazyEither[E, *]] with MonadError[LazyEither[E, *], E] =
    new Traverse[LazyEither[E, *]] with Monad[LazyEither[E, *]] with BindRec[LazyEither[E, *]] with Cozip[LazyEither[E, *]] with Optional[LazyEither[E, *]] with MonadError[LazyEither[E, *], E] {

      def traverseImpl[G[_]: Applicative, A, B](fa: LazyEither[E, A])(f: A => G[B]): G[LazyEither[E, B]] =
        fa traverse f

      override def foldRight[A, B](fa: LazyEither[E, A], z: => B)(f: (A, => B) => B): B =
        fa.foldRight(z)(f)

      def bind[A, B](fa: LazyEither[E, A])(f: A => LazyEither[E, B]): LazyEither[E, B] =
        fa flatMap (a => f(a))

      override def ap[A, B](fa: => LazyEither[E, A])(f: => LazyEither[E, A => B]): LazyEither[E, B] =
        fa ap f

      def point[A](a: => A): LazyEither[E, A] =
        LazyEither.lazyRight(a)

      def cozip[A, B](a: LazyEither[E, A \/ B]): LazyEither[E, A] \/ LazyEither[E, B] =
        a.fold(
          e => -\/(LazyEither.lazyLeft(e))
        , {
            case -\/(a) => -\/(LazyEither.lazyRight(a))
            case \/-(b) => \/-(LazyEither.lazyRight(b))
          }
        )

      def pextract[B, A](fa: LazyEither[E,A]): LazyEither[E,B] \/ A =
        fa.fold(e => -\/(LazyEither.lazyLeft(e)), a => \/-(a))

      def raiseError[A](e: E): LazyEither[E, A] =
        LazyEither.lazyLeft(e)

      def handleError[A](fa: LazyEither[E, A])(f: E => LazyEither[E, A]): LazyEither[E, A] =
        fa.left.flatMap(e => f(e))

      @annotation.tailrec
      def tailrecM[A, B](f: A => LazyEither[E, A \/ B])(a: A): LazyEither[E, B] =
        f(a) match {
          case LazyLeft(l) => LazyLeft(l)
          case LazyRight(r) => r() match {
            case \/-(b) => LazyEither.lazyRight(b)
            case -\/(a0) => tailrecM(f)(a0)
          }
        }
    }

  implicit val lazyEitherAssociative: Associative[LazyEither] = new Associative[LazyEither] {
    def reassociateLeft[A, B, C](f: LazyEither[A, LazyEither[B, C]]) =
      f.fold(
        a => LazyEither.lazyLeft(LazyEither.lazyLeft(a)),
        _.fold(
          b => LazyEither.lazyLeft(LazyEither.lazyRight(b)),
          LazyEither.lazyRight(_)
        )
      )

    def reassociateRight[A, B, C](f: LazyEither[LazyEither[A, B], C]) =
      f.fold(
        _.fold(
          LazyEither.lazyLeft(_),
          b => LazyEither.lazyRight(LazyEither.lazyLeft(b))
        ),
        c => LazyEither.lazyRight(LazyEither.lazyRight(c))
      )
  }

  implicit val lazyEitherBitraverse: Bitraverse[LazyEither] = new Bitraverse[LazyEither] {
    override def bimap[A, B, C, D](fab: LazyEither[A, B])(f: A => C, g: B => D) =
      fab.map(x => g(x)).left.map(x => f(x))

    def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: LazyEither[A, B])
                                                  (f: A => G[C], g: B => G[D]): G[LazyEither[C, D]] =
      fab.fold(
        a => Applicative[G].map(f(a))(b => LazyEither.lazyLeft[D](b)),
        b => Applicative[G].map(g(b))(d => LazyEither.lazyRight[C](d))
      )
  }
}
