package scalaz

/** [[scala.Option]], but with a value by name. */
sealed trait LazyOption[+A] {

  import LazyOption._
  import LazyEither._
//  import newtypes.{FirstLazyOption, LastLazyOption}

  def fold[X](some: (=> A) => X, none: => X): X =
    this match {
      case LazySome(z) => some(z())
      case LazyNone    => none
    }

  def ?[X](some: => X, none: => X): X =
    fold(_ => some, none)

  def isDefined =
    fold(_ => true, false)

  def isEmpty =
    !isDefined

  def getOrElse[AA >: A](default: => AA): AA =
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

  def toRight[X](left: => X): (X \/ A) =
    fold(\/-(_), -\/(left))

  def toLeft[X](right: => X): (A \/ X) =
    fold(-\/(_), \/-(right))

  def toList: List[A] =
    fold(List(_), Nil)

  def orElse[AA >: A](a: => LazyOption[AA]): LazyOption[AA] =
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

  def ap[B](f: => LazyOption[A => B]): LazyOption[B] =
    fold(a => f map (k => k.apply(a)), lazyNone)

  def traverse[G[_] : Applicative, B](f: (=> A) => G[B]): G[LazyOption[B]] =
    fold(
      some = x => Applicative[G].map(f(x))(b => lazySome(b)),
      none = Applicative[G].point(lazyNone[B])
    )

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    fold(
      some = a => f(a, z),
      none = z
    )

  def zip[B](b: => LazyOption[B]): LazyOption[(A, B)] =
    for {
      x <- this
      y <- b
    } yield (x, y)

  def unzip[X, Y](implicit ev: A <:< (X, Y)): (LazyOption[X], LazyOption[Y]) =
    fold(xy => (lazySome(xy._1), lazySome(xy._2)), (lazyNone, lazyNone))

}

private case class LazySome[A](a: () => A) extends LazyOption[A]

private case object LazyNone extends LazyOption[Nothing]

object LazyOption extends LazyOptionFunctions with LazyOptionInstances

trait LazyOptionInstances {
  import LazyOption._

  implicit val lazyOptionInstance = new Traverse[LazyOption] with MonadPlus[LazyOption] with Cozip[LazyOption] with Zip[LazyOption] with Unzip[LazyOption] with Cobind[LazyOption] with Optional[LazyOption] {
    def cobind[A, B](fa: LazyOption[A])(f: LazyOption[A] => B): LazyOption[B] = map(cojoin(fa))(f)
    override def cojoin[A](a: LazyOption[A]) = a match {
      case LazyNone => LazyNone
      case o @ LazySome(_) => LazySome(() => o)
    }
    def traverseImpl[G[_]: Applicative, A, B](fa: LazyOption[A])(f: A => G[B]): G[LazyOption[B]] =  fa traverse (a => f(a))
    override def foldRight[A, B](fa: LazyOption[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
    override def ap[A, B](fa: => LazyOption[A])(f: => LazyOption[A => B]): LazyOption[B] = fa ap f
    def plus[A](a: LazyOption[A], b: => LazyOption[A]): LazyOption[A] = a orElse b
    def bind[A, B](fa: LazyOption[A])(f: A => LazyOption[B]): LazyOption[B] = fa flatMap (a => f(a))
    def point[A](a: => A): LazyOption[A] = lazySome(a)
    def empty[A]: LazyOption[A] = lazyNone
    def cozip[A, B](a: LazyOption[A \/ B]) =
      a.fold({
        case -\/(a) => -\/(lazySome(a))
        case \/-(b) => \/-(lazySome(b))
      }, -\/(lazyNone))
    def zip[A, B](a: => LazyOption[A], b: => LazyOption[B]) = a zip b
    def unzip[A, B](a: LazyOption[(A, B)]) = a.unzip
    def pextract[B, A](fa: LazyOption[A]): LazyOption[B] \/ A =
      fa.fold(a => \/-(a), -\/(lazyNone))
    override def isDefined[A](fa: LazyOption[A]): Boolean = fa.isDefined
  }

  implicit def lazyOptionEqual[A: Equal]: Equal[LazyOption[A]] = {
    import std.option._
    Equal.equalBy(_.toOption)
  }

  implicit def lazyOptionShow[A](implicit S: Show[A]): Show[LazyOption[A]] =
    Show.shows(_.fold(a ⇒ "LazySome(%s)".format(S.shows(a)), "LazyNone"))

  /* TODO
implicit def LazyOptionOrder[A: Order]: Order[LazyOption[A]] =
  Order.orderBy(_.toOption)*/
}

trait LazyOptionFunctions {
  def lazySome[A](a: => A): LazyOption[A] =
    LazySome(() => a)

  def lazyNone[A]: LazyOption[A] =
    LazyNone

  def fromOption[A](oa: Option[A]): LazyOption[A] = oa match {
    case Some(x) => lazySome(x)
    case None    => lazyNone[A]
  }

  /**
   * Returns the given argument in `lazySome` if this is `true`, `lazyNone` otherwise.
   */
  def condLazyOption[A](value: Boolean, a: => A): LazyOption[A] = if (value) lazySome(a) else lazyNone
}
