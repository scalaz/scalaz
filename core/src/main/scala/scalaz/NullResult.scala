package scalaz

sealed trait NullResult[A, B] {
  def apply(a: A): Option[B]

  import NullResult._

  def map[C](f: B => C): A =>? C =
    NullResult(apply(_) map f)

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
}

object NullResult extends NullResultFunctions

trait NullResultFunctions {
  type =>?[A, B] = NullResult[A, B]
  def apply[A, B](f: A => Option[B]): A =>? B =
    new (A =>? B) {
      def apply(a: A) = f(a)
    }

  def lift[A, B](f: A => B): A =>? B =
    apply(a => Some(f(a)))

  def always[A, B](b: => B): A =>? B =
    lift(_ => b)

  def never[A, B]: A =>? B =
    apply(_ => None)

  def zero[A, B](implicit M: Monoid[B]): A =>? B =
    always(M.zero)
}
