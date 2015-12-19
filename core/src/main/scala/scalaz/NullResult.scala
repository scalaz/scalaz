package scalaz

final class NullResult[A, B] private(_apply: A => Option[B]) {
  def apply(a: A): Option[B] = _apply(a)

  import NullResult._
  import NullArgument._

  def dimap[C, D](f: C => A, g: B => D): NullResult[C, D] =
    NullResult(c => apply(f(c)) map g)

  def map[C](f: B => C): A =>? C =
    NullResult(apply(_) map f)

  def contramap[C](f: C => A): C =>? B =
    NullResult(f andThen _apply)

  def flatMap[C](f: B => A =>? C): A =>? C =
    NullResult(a => apply(a) flatMap(f(_)(a)))

  def ap[C](f: A =>? (B => C)): A =>? C =
    for {
      ff <- f
      bb <- this
    } yield ff(bb)

  def zip[C](x: A =>? C): A =>? (B, C) =
    for {
      b <- this
      c <- x
    } yield (b, c)

  def ***[C, D](x: C =>? D): (A, C) =>? (B, D) =
    NullResult {
      case (a, c) => apply(a) flatMap (b => x(c) map (d => (b, d)))
    }

  def +++[C, D](x: C =>? D): (A \/ C) =>? (B \/ D) =
    NullResult {
      case -\/(a) => apply(a) map (\/.left)
      case \/-(c) => x(c) map (\/.right)
    }

  def first[C]: (A, C) =>? (B, C) =
    NullResult {
      case (a, c) => apply(a) map (b => (b, c))
    }

  def second[C]: (C, A) =>? (C, B) =
    NullResult {
      case (c, a) => apply(a) map (b => (c, b))
    }

  def left[C]: (A \/ C) =>? (B \/ C) =
    NullResult {
      case -\/(a) => apply(a) map (\/.left)
      case c @ \/-(_) => Some(c)
    }

  def right[C]: (C \/ A) =>? (C \/ B) =
    NullResult {
      case c @ -\/(_) => Some(c)
      case \/-(a) => apply(a) map (\/.right)
    }

  def |(x: => A =>? B): A =>? B =
    NullResult(a => apply(a) orElse x(a))

  def compose[C](f: C =>? A): C =>? B =
    NullResult(f(_) flatMap _apply)

  def andThen[C](g: B =>? C): A =>? C =
    g compose this

  def |+|(x: A =>? B)(implicit S: Semigroup[B]): A =>? B =
    for {
      b1 <- this
      b2 <- x
    } yield S.append(b1, b2)

  def =>>[C](f: B ?=> C): A =>? C =
    NullResult(apply(_) map (b => f(Some(b))))

  def isDefinedAt(a: A): Boolean =
    apply(a).isDefined

  def isEmptyAt(a: A): Boolean =
    apply(a).isEmpty

  def or(a: A, b: => B): B =
    apply(a) getOrElse b

  def carry: A =>? (A, B) =
    NullResult(a => apply(a) map (b => (a, b)))

  def cancel: A =>? A =
    carry map (_._1)

  def kleisli: Kleisli[Option, A, B] =
    Kleisli(_apply)

  import std.option._

  def state: StateT[Option, A, B] =
    StateT(carry apply _)

  def traverse[F[_]](a: F[A])(implicit T: Traverse[F]): Option[F[B]] =
    T.traverse(a)(_apply)

  def on[F[_]](a: F[A])(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(a)(_apply))
}

object NullResult extends NullResultInstances {
  def apply[A, B](f: A => Option[B]): A =>? B =
    new (A =>? B)(f)

  type =>?[A, B] = NullResult[A, B]

  def kleisli[A, B](k: Kleisli[Option, A, B]): A =>? B =
    NullResult(k.run)

  def lift[A, B](f: A => B): A =>? B =
    NullResult(a => Some(f(a)))

  def always[A, B](b: => B): A =>? B =
    lift(_ => b)

  def never[A, B]: A =>? B =
    NullResult(_ => None)

  def zero[A, B](implicit M: Monoid[B]): A =>? B =
    always(M.zero)

  object list {
    def head[A]: List[A] =>? A =
      NullResult(_.headOption)

    def tail[A]: List[A] =>? List[A] =
      NullResult {
        case Nil => None
        case _::t => Some(t)
      }
  }
}

sealed abstract class NullResultInstances0 {

  implicit def nullResultSemigroup[A, B](implicit M0: Semigroup[B]): Semigroup[NullResult[A, B]] =
    new NullResultSemigroup[A, B] {
      implicit val M = M0
    }

  implicit val nullResultProfunctor: Profunctor[NullResult] =
    new Profunctor[NullResult] {
      def mapfst[A, B, C](fab: NullResult[A, B])(f: C => A) =
        fab contramap f
      def mapsnd[A, B, C](fab: NullResult[A, B])(f: B => C) =
        fab map f
      override def dimap[A, B, C, D](fab: NullResult[A, B])(f: C => A)(g: B => D) =
        fab.dimap(f, g)
    }

}

sealed abstract class NullResultInstances extends NullResultInstances0 {

  implicit def nullResultMonoid[A, B](implicit M0: Monoid[B]): Monoid[NullResult[A, B]] =
    new NullResultMonoid[A, B] {
      implicit val M = M0
    }

  implicit val nullResultArrow: Arrow[NullResult] with Choice[NullResult] with ProChoice[NullResult] =
    new Arrow[NullResult] with Choice[NullResult] with ProChoice[NullResult] {
      def id[A] =
        NullResult.lift(identity)
      override def compose[A, B, C](f: NullResult[B, C], g: NullResult[A, B]): NullResult[A, C] =
        f compose g
      override def split[A, B, C, D](f: NullResult[A, B], g: NullResult[C, D]) =
        f *** g
      override def mapfst[A, B, C](r: NullResult[A, B])(f: C => A) =
        r contramap f
      override def mapsnd[A, B, C](r: NullResult[A, B])(f: B => C) =
        r map f
      override def arr[A, B](f: A => B) =
        NullResult.lift(f)
      override def first[A, B, C](r: NullResult[A, B]) =
        r.first
      override def left[A, B, C](fa: NullResult[A, B]) =
        fa.left
      override def right[A, B, C](fa: NullResult[A, B]) =
        fa.right
      override def choice[A, B, C](f: => NullResult[A, C], g: => NullResult[B, C]) =
        NullResult{
          case \/-(a) => g(a)
          case -\/(a) => f(a)
        }
    }

  implicit def nullResultMonadPlus[X]: MonadPlus[NullResult[X, ?]] with BindRec[NullResult[X, ?]] =
    new MonadPlus[NullResult[X, ?]] with BindRec[NullResult[X, ?]] {
      import std.option._
      override def tailrecM[A, B](f: A => NullResult[X, A \/ B])(a: A) =
        NullResult(r => BindRec[Option].tailrecM(f(_: A)(r))(a))
      override def map[A, B](a: NullResult[X, A])(f: A => B) =
        a map f
      override def ap[A, B](a: => NullResult[X, A])(f: => NullResult[X, A => B]) =
        a ap f
      override def point[A](a: => A): NullResult[X, A] =
        NullResult.always(a)
      override def bind[A, B](a: NullResult[X, A])(f: A => NullResult[X, B]) =
        a flatMap f
      override def empty[A] =
        NullResult.never[X, A]
      override def plus[A](a: NullResult[X, A], b: => NullResult[X, A]) =
        NullResult[X, A](x => a(x) orElse b(x))
    }

  implicit def nullResultContravariant[X]: Contravariant[NullResult[?, X]] =
    new Contravariant[NullResult[?, X]] {
      override def contramap[A, B](a: NullResult[A, X])(f: B => A) =
        a contramap f
    }
}

private trait NullResultSemigroup[A, B] extends Semigroup[NullResult[A, B]] {
  implicit val M: Semigroup[B]

  override def append(a1: NullResult[A, B], a2: => NullResult[A, B]) =
    a1 |+| a2
}

private trait NullResultMonoid[A, B] extends Monoid[NullResult[A, B]] with NullResultSemigroup[A, B] {
  implicit val M: Monoid[B]

  override def zero =
    NullResult.zero
}

// vim: expandtab:ts=2:sw=2
