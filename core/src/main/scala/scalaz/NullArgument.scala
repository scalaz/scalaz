package scalaz

trait NullArgument[A, B] {
  def apply(a: Option[A]): B

  import NullResult._
  import NullArgument._

  def map[C](f: B => C): A ?=> C =
    NullArgument(a => f(apply(a)))

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

object NullArgument extends NullArgumentFunctions

trait NullArgumentFunctions {
  type ?=>[A, B] = NullArgument[A, B]
  def apply[A, B](f: Option[A] => B): A ?=> B =
    new (A ?=> B) {
      def apply(a: Option[A]) = f(a)
    }

  def pair[A, B](f: A => B, b: => B): A ?=> B =
    apply((_: Option[A]) match {
      case None => b
      case Some(a) => f(a)
    })

  def cokleisli[A, B](c: Cokleisli[Option, A, B]): A ?=> B =
    apply(c apply _)
}
