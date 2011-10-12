package scalaz
package std

trait Options {
  implicit val option = new MonadPlus[Option] with Traverse[Option] {
    def pure[A](a: => A) = Some(a)

    def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f

    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f

    def traverseImpl[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]) =
      fa map (a => F.map(f(a))(Some(_): Option[B])) getOrElse F.pure(None)

    def empty[A]: Option[A] = None

    def plus[A](a: Option[A], b: => Option[A]) = a orElse b
  }

  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def append(f1: Option[A], f2: => Option[A]): Option[A] = (f1, f2) match {
      case (Some(a1), Some(a2)) => Some(Semigroup[A].append(a1, a2))
      case (Some(a1), None) => f1
      case (None, Some(a2)) => f2
      case (None, None) => None
    }

    def zero: Option[A] = None
  }

  sealed trait First

  sealed trait Last

  implicit def optionFirst[A] = new Monoid[Option[A] @@ First] {
    def zero: Option[A] @@ First = Tag(None)

    def append(f1: Option[A] @@ First, f2: => Option[A] @@ First) = Tag(f1.orElse(f2))
  }

  implicit def optionLast[A] = new Monoid[Option[A] @@ Last] {
    def zero: Option[A] @@ Last = Tag(None)

    def append(f1: Option[A] @@ Last, f2: => Option[A] @@ Last) = Tag(f2.orElse(f1))
  }

  def some[A](a: A): Option[A] = Some(a)
  def none[A]: Option[A] = None
}

object Option extends Options

