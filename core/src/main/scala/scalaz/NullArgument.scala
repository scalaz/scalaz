package scalaz

final class NullArgument[A, B] private(_apply: Option[A] => B) {
  def apply(a: Option[A]): B = _apply(a)

  import NullArgument._

  def dimap[C, D](f: C => A, g: B => D): NullArgument[C, D] =
    NullArgument(c => g(_apply(c.map(f))))

  def map[C](f: B => C): A ?=> C =
    NullArgument(_apply andThen f)

  def contramap[C](f: C => A): C ?=> B =
    NullArgument(c => apply(c.map(f)))

  def flatMap[C](f: B => A ?=> C): A ?=> C =
    NullArgument(a => f(apply(a))(a))

  def ap[C](f: A ?=> (B => C)): A ?=> C =
    for {
      ff <- f
      bb <- this
    } yield ff(bb)

  def zip[C](x: A ?=> C): A ?=> (B, C) =
    for {
      b <- this
      c <- x
    } yield (b, c)

  def ***[C, D](x: C ?=> D): (A, C) ?=> (B, D) =
    NullArgument {
      case None => (apply(None), x(None))
      case Some((a, c)) => (apply(Some(a)), x(Some(c)))
    }

  def +++[C, D](x: C ?=> D): (A \/ C) ?=> (B \/ D) =
    left compose x.right

  def left[C]: (A \/ C) ?=> (B \/ C) =
    NullArgument {
      case None => -\/(apply(None))
      case Some(-\/(a)) => -\/(apply(Some(a)))
      case Some(c @ \/-(_)) => c
    }

  def right[C]: (C \/ A) ?=> (C \/ B) =
    NullArgument {
      case None => \/-(apply(None))
      case Some(\/-(a)) => \/-(apply(Some(a)))
      case Some(c @ -\/(_)) => c
    }

  def compose[C](f: C ?=> A): C ?=> B =
    NullArgument {
      case None => apply(None)
      case c @ Some(_) => apply(Some(f(c)))
    }

  def andThen[C](g: B ?=> C): A ?=> C =
    g compose this

  def |+|(x: A ?=> B)(implicit S: Semigroup[B]): A ?=> B =
    for {
      b1 <- this
      b2 <- x
    } yield S.append(b1, b2)

  def cokleisli: Cokleisli[Option, A, B] =
    Cokleisli(_apply)

  def on[F[_]](o: OptionT[F, A])(implicit F: Functor[F]): F[B] =
    F.map(o.run)(_apply)

  def lower: A => B =
    a => apply(Some(a))

  def never: B =
    apply(None)

  def zero(implicit M: Monoid[A]): B =
    lower(M.zero)

  def pair: (A => B, B) =
    (a => apply(Some(a)), apply(None))

}

object NullArgument extends NullArgumentInstances {
  def apply[A, B](f: Option[A] => B): A ?=> B =
    new (A ?=> B)(f)

  type ?=>[A, B] = NullArgument[A, B]

  def always[A, B](b: => B): A ?=> B =
    NullArgument(_ => b)

  def zero[A, B](implicit M: Monoid[B]): A ?=> B =
    always(M.zero)

  def pair[A, B](f: A => B, b: => B): A ?=> B =
    NullArgument((_: Option[A]) match {
      case None => b
      case Some(a) => f(a)
    })

  def cokleisli[A, B](c: Cokleisli[Option, A, B]): A ?=> B =
    NullArgument(c.run)
}

sealed abstract class NullArgumentInstances0 {

  implicit def nullArgumentSemigroup[A, B](implicit M0: Semigroup[B]): Semigroup[NullArgument[A, B]] =
    new NullArgumentSemigroup[A, B] {
      implicit val M = M0
    }

  implicit val nullArgumentProfunctor: Profunctor[NullArgument] =
    new Profunctor[NullArgument] {
      def mapfst[A, B, C](fab: NullArgument[A, B])(f: C => A) =
        fab contramap f
      def mapsnd[A, B, C](fab: NullArgument[A, B])(f: B => C) =
        fab map f
      override def dimap[A, B, C, D](fab: NullArgument[A, B])(f: C => A)(g: B => D) =
        fab.dimap(f, g)
    }

}

sealed abstract class NullArgumentInstances extends NullArgumentInstances0 {

  implicit def nullArgumentMonoid[A, B](implicit M0: Monoid[B]): Monoid[NullArgument[A, B]] =
    new NullArgumentMonoid[A, B] {
      implicit val M = M0
    }

  implicit val nullArgumentCategory: Split[NullArgument] with Profunctor[NullArgument] =
    new Split[NullArgument] with Profunctor[NullArgument] {
      override def compose[A, B, C](f: NullArgument[B, C], g: NullArgument[A, B]): NullArgument[A, C] =
        f compose g
      override def split[A, B, C, D](f: NullArgument[A, B], g: NullArgument[C, D]) =
        f *** g
      override def mapfst[A, B, C](r: NullArgument[A, B])(f: C => A) =
        r contramap f
      override def mapsnd[A, B, C](r: NullArgument[A, B])(f: B => C) =
        r map f
    }

  implicit def nullArgumentMonad[X]: Monad[NullArgument[X, ?]] with BindRec[NullArgument[X, ?]] =
    new Monad[NullArgument[X, ?]] with BindRec[NullArgument[X, ?]] {
      override def ap[A, B](a: => NullArgument[X, A])(f: => NullArgument[X, A => B]) =
        a ap f
      override def map[A, B](a: NullArgument[X, A])(f: A => B) =
        a map f
      override def point[A](a: => A): NullArgument[X, A] =
        NullArgument.always(a)
      override def bind[A, B](a: NullArgument[X, A])(f: A => NullArgument[X, B]) =
        a flatMap f
      override def tailrecM[A, B](f: A => NullArgument[X, A \/ B])(a: A) =
        NullArgument{ t =>
          @annotation.tailrec
          def go(a0: A): B =
            f(a0)(t) match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }

  implicit def nullArgumentContravariant[X]: Contravariant[NullArgument[?, X]] =
    new Contravariant[NullArgument[?, X]] {
      override def contramap[A, B](a: NullArgument[A, X])(f: B => A) =
        a contramap f
    }
}

private trait NullArgumentSemigroup[A, B] extends Semigroup[NullArgument[A, B]] {
  implicit val M: Semigroup[B]

  override def append(a1: NullArgument[A, B], a2: => NullArgument[A, B]) =
    a1 |+| a2
}

private trait NullArgumentMonoid[A, B] extends Monoid[NullArgument[A, B]] with NullArgumentSemigroup[A, B] {
  implicit val M: Monoid[B]

  override def zero =
    NullArgument.zero
}

// vim: expandtab:ts=2:sw=2
