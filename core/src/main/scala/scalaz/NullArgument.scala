package scalaz

trait NullArgument[A, B] {
  def apply(a: Option[A]): B

  import NullResult._
  import NullArgument._

  def map[C](f: B => C): A ?=> C =
    NullArgument(a => f(apply(a)))

  def contramap[C](f: C => A): C ?=> B =
    ???

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
      case Some(\/-(c)) => \/-(c)
    }

  def right[C]: (C \/ A) ?=> (C \/ B) =
    NullArgument {
      case None => \/-(apply(None))
      case Some(\/-(a)) => \/-(apply(Some(a)))
      case Some(-\/(c)) => -\/(c)
    }

  def compose[C](f: C ?=> A): C ?=> B =
    NullArgument {
      case None => apply(None)
      case Some(c) => apply(Some(f(Some(c))))
    }

  def andThen[C](g: B ?=> C): A ?=> C =
    g compose this

  def |+|(x: A ?=> B)(implicit S: Semigroup[B]): A ?=> B =
    for {
      b1 <- this
      b2 <- x
    } yield S.append(b1, b2)

  def cokleisli: Cokleisli[Option, A, B] =
    Cokleisli(apply(_))

  def on[F[+_]](o: OptionT[F, A])(implicit F: Functor[F]): F[B] =
    F.map(o.run)(apply(_))

  def lower: A => B =
    a => apply(Some(a))

  def never: B =
    apply(None)

  def zero(implicit M: Monoid[A]): B =
    lower(M.zero)

  def pair: (A => B, B) =
    (a => apply(Some(a)), apply(None))

}

object NullArgument extends NullArgumentFunctions with NullArgumentInstances

trait NullArgumentFunctions {
  type ?=>[A, B] = NullArgument[A, B]
  def apply[A, B](f: Option[A] => B): A ?=> B =
    new (A ?=> B) {
      def apply(a: Option[A]) = f(a)
    }

  def always[A, B](b: => B): A ?=> B =
    apply(_ => b)

  def zero[A, B](implicit M: Monoid[B]): A ?=> B =
    always(M.zero)

  def pair[A, B](f: A => B, b: => B): A ?=> B =
    apply((_: Option[A]) match {
      case None => b
      case Some(a) => f(a)
    })

  def cokleisli[A, B](c: Cokleisli[Option, A, B]): A ?=> B =
    apply(c apply _)
}

trait NullArgumentInstances {
  implicit def NullArgumentSemigroup[A, B](implicit M0: Semigroup[B]): Semigroup[NullArgument[A, B]] =
    new NullArgumentSemigroup[A, B] {
      implicit val M = M0
    }

  implicit def NullArgumentFunctor[X]: Functor[({type λ[α] = NullArgument[X, α]})#λ] =
    new NullArgumentFunctor[X] {
    }

  implicit def NullArgumentContravariant[X]: Contravariant[({type λ[α] = NullArgument[α, X]})#λ] =
    new NullArgumentContravariant[X] {
    }

  implicit def NullArgumentCompose: Compose[NullArgument] =
    new NullArgumentCompose {
    }

  implicit def NullArgumentProfunctor: Profunctor[NullArgument] =
    new NullArgumentProfunctor {
    }
}

trait NullArgumentInstances0 extends NullArgumentInstances {
  implicit def NullArgumentMonoid[A, B](implicit M0: Monoid[B]): Monoid[NullArgument[A, B]] =
    new NullArgumentMonoid[A, B] {
      implicit val M = M0
    }

  implicit def NullArgumentApply[X]: Apply[({type λ[α] = NullArgument[X, α]})#λ] =
    new NullArgumentApply[X] {
    }
}

trait NullArgumentInstances1 extends NullArgumentInstances0 {
  implicit def NullArgumentApplicative[X]: Applicative[({type λ[α] = NullArgument[X, α]})#λ] =
    new NullArgumentApplicative[X] {
    }

  implicit def NullArgumentSplit: Split[NullArgument] =
    new NullArgumentSplit {
    }
}

trait NullArgumentInstances2 extends NullArgumentInstances1 {
  implicit def NullArgumentMonad[X]: Monad[({type λ[α] = NullArgument[X, α]})#λ] =
    new NullArgumentMonad[X] {
    }
}

private[scalaz] trait NullArgumentSemigroup[A, B] extends Semigroup[NullArgument[A, B]] {
  implicit val M: Semigroup[B]

  override def append(a1: NullArgument[A, B], a2: => NullArgument[A, B]) =
    a1 |+| a2
}

private[scalaz] trait NullArgumentMonoid[A, B] extends Monoid[NullArgument[A, B]] with NullArgumentSemigroup[A, B] {
  implicit val M: Monoid[B]

  override def zero =
    NullArgument.zero
}

private[scalaz] trait NullArgumentFunctor[X] extends Functor[({type λ[α] = NullArgument[X, α]})#λ] {
  override def map[A, B](a: NullArgument[X, A])(f: A => B) =
    a map f
}

private[scalaz] trait NullArgumentContravariant[X] extends Contravariant[({type λ[α] = NullArgument[α, X]})#λ] {
  override def contramap[A, B](a: NullArgument[A, X])(f: B => A) =
    a contramap f
}

private[scalaz] trait NullArgumentApply[X] extends Apply[({type λ[α] = NullArgument[X, α]})#λ] with NullArgumentFunctor[X] {
  override def ap[A, B](a: => NullArgument[X, A])(f: => NullArgument[X, A => B]) =
    a ap f
}

private[scalaz] trait NullArgumentApplicative[X] extends Applicative[({type λ[α] = NullArgument[X, α]})#λ] with NullArgumentApply[X] {
  override def map[A, B](a: NullArgument[X, A])(f: A => B) =
    a map f
  override def ap[A, B](a: => NullArgument[X, A])(f: => NullArgument[X, A => B]) =
    a ap f
  override def point[A](a: => A): NullArgument[X, A] =
    NullArgument.always(a)
}

private[scalaz] trait NullArgumentMonad[X] extends Monad[({type λ[α] = NullArgument[X, α]})#λ] with NullArgumentApplicative[X] {
  override def bind[A, B](a: NullArgument[X, A])(f: A => NullArgument[X, B]) =
    a flatMap f
}

private[scalaz] trait NullArgumentCompose extends Compose[NullArgument] {
  override def compose[A, B, C](f: NullArgument[B, C], g: NullArgument[A, B]): NullArgument[A, C] =
    f compose g
}

private[scalaz] trait NullArgumentSplit extends Split[NullArgument] with NullArgumentCompose {
  override def split[A, B, C, D](f: NullArgument[A, B], g: NullArgument[C, D]) =
    f *** g
}

private[scalaz] trait NullArgumentProfunctor extends Profunctor[NullArgument] {
  override def mapfst[A, B, C](r: NullArgument[A, B])(f: C => A) =
    r contramap f
  override def mapsnd[A, B, C](r: NullArgument[A, B])(f: B => C) =
    r map f
}
