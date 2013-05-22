package scalaz

sealed trait NullResult[A, B] {
  def apply(a: A): Option[B]

  import NullResult._
  import NullArgument._

  def map[C](f: B => C): A =>? C =
    NullResult(apply(_) map f)

  def contramap[C](f: C => A): C =>? B =
    NullResult(c => apply(f(c)))

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
      case -\/(a) => apply(a) map (-\/(_))
      case \/-(c) => x(c) map (\/-(_))
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
      case -\/(a) => apply(a) map (-\/(_))
      case \/-(c) => Some(\/-(c))
    }

  def right[C]: (C \/ A) =>? (C \/ B) =
    NullResult {
      case -\/(c) => Some(-\/(c))
      case \/-(a) => apply(a) map (\/-(_))
    }

  def |(x: => A =>? B): A =>? B =
    NullResult(a => apply(a) orElse x(a))

  def compose[C](f: C =>? A): C =>? B =
    NullResult(f(_) flatMap (apply(_)))

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
    Kleisli(apply(_))

  import std.option._

  def state: StateT[Option, A, B] =
    StateT(carry apply _)

  def traverse[F[_]](a: F[A])(implicit T: Traverse[F]): Option[F[B]] =
    T.traverse(a)(apply(_))

  def on[F[+_]](a: F[A])(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(a)(apply(_)))
}

object NullResult extends NullResultFunctions with NullResultInstances

trait NullResultFunctions {
  type =>?[A, B] = NullResult[A, B]
  def apply[A, B](f: A => Option[B]): A =>? B =
    new (A =>? B) {
      def apply(a: A) = f(a)
    }

  def kleisli[A, B](k: Kleisli[Option, A, B]): A =>? B =
    apply(k apply _)

  def lift[A, B](f: A => B): A =>? B =
    apply(a => Some(f(a)))

  def always[A, B](b: => B): A =>? B =
    lift(_ => b)

  def never[A, B]: A =>? B =
    apply(_ => None)

  def zero[A, B](implicit M: Monoid[B]): A =>? B =
    always(M.zero)

  object list {
    def head[A]: List[A] =>? A =
      apply(_.headOption)

    def tail[A]: List[A] =>? List[A] =
      apply {
        case Nil => None
        case _::t => Some(t)
      }
  }
}

trait NullResultInstances0 {

  implicit def nullResultSemigroup[A, B](implicit M0: Semigroup[B]): Semigroup[NullResult[A, B]] =
    new NullResultSemigroup[A, B] {
      implicit val M = M0
    }

}

trait NullResultInstances extends NullResultInstances0 {

  implicit def nullResultMonoid[A, B](implicit M0: Monoid[B]): Monoid[NullResult[A, B]] =
    new NullResultMonoid[A, B] {
      implicit val M = M0
    }

  implicit def nullResultArrow: Arrow[NullResult] = new Arrow[NullResult] {
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
  }

  implicit def nullResultMonad[X]: Monad[({type λ[α] = NullResult[X, α]})#λ] = new Monad[({type λ[α] = NullResult[X, α]})#λ] {
    override def map[A, B](a: NullResult[X, A])(f: A => B) =
      a map f
    override def ap[A, B](a: => NullResult[X, A])(f: => NullResult[X, A => B]) =
      a ap f
    override def point[A](a: => A): NullResult[X, A] =
      NullResult.always(a)
    override def bind[A, B](a: NullResult[X, A])(f: A => NullResult[X, B]) =
      a flatMap f
  }

  implicit def nullResultContravariant[X]: Contravariant[({type λ[α] = NullResult[α, X]})#λ] = new Contravariant[({type λ[α] = NullResult[α, X]})#λ] {
    override def contramap[A, B](a: NullResult[A, X])(f: B => A) =
      a contramap f
  }

}

private[scalaz] trait NullResultSemigroup[A, B] extends Semigroup[NullResult[A, B]] {
  implicit val M: Semigroup[B]

  override def append(a1: NullResult[A, B], a2: => NullResult[A, B]) =
    a1 |+| a2
}

private[scalaz] trait NullResultMonoid[A, B] extends Monoid[NullResult[A, B]] with NullResultSemigroup[A, B] {
  implicit val M: Monoid[B]

  override def zero =
    NullResult.zero
}

// vim: expandtab:ts=2:sw=2
