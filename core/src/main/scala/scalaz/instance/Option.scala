package scalaz
package instance

trait Options {
  implicit val option = new MonadPlusInstance[Option] with TraverseInstance[Option] {
    def pure[A](a: => A) = Some(a)
    def bind[A,B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
    override def map[A,B](fa: Option[A])(f: A => B): Option[B] = fa map f
    def traverseImpl[F[_],A,B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]) = 
      fa map (a => F.map(f(a))(Some(_): Option[B])) getOrElse F.pure(None) 
    def empty[A]: Option[A] = None
    def plus[A](a: Option[A], b: => Option[A]) = a orElse b
  }

  implicit def optionSemigroup[A: Semigroup] = new Semigroup[Option[A]] {
    def append(f1: Option[A], f2: => Option[A]): Option[A] = (f1, f2) match {
      case (Some(a1), Some(a2)) => Some(Semigroup[A].append(a1, a2))
      case (Some(a1), None) => f1
      case (None, Some(a2)) => f2
      case (None, None) => None
    }
  }

  // TODO duplication with optionSemigroup
  implicit def optionMonoid[A: Monoid] = new Monoid[Option[A]] {
    def append(f1: Option[A], f2: => Option[A]): Option[A] = (f1, f2) match {
      case (Some(a1), Some(a2)) => Some(Monoid[A].append(a1, a2))
      case (Some(a1), None) => f1
      case (None, Some(a2)) => f2
      case (None, None) => None
    }

    def zero: Option[A] = Some(Monoid[A].zero)
  }
}

object Option extends Options

