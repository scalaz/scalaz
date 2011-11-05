package scalaz
package std

trait OptionInstances {
  implicit val optionInstance = new Traverse[Option] with MonadPlus[Option] with Each[Option] with Index[Option] with Length[Option] {
    def point[A](a: => A) = Some(a)
    def each[A](fa: Option[A])(f: (A) => Unit) = fa foreach f
    def index[A](fa: Option[A], n: Int): Option[A] = if (n == 0) fa else None
    def length[A](fa: Option[A]): Int = if (fa.isEmpty) 0 else 1
    def bind[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa flatMap f
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
    def traverseImpl[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]) =
      fa map (a => F.map(f(a))(Some(_): Option[B])) getOrElse F.point(None)
    def empty[A]: Option[A] = None
    def plus[A](a: Option[A], b: => Option[A]) = a orElse b
    def foldR[A, B](fa: Option[A], z: B)(f: (A) => (=> B) => B): B = fa match {
      case Some(a) => f(a)(z)
      case None    => z
    }
  }

  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def append(f1: Option[A], f2: => Option[A]): Option[A] = (f1, f2) match {
      case (Some(a1), Some(a2)) => Some(Semigroup[A].append(a1, a2))
      case (Some(a1), None)     => f1
      case (None, Some(a2))     => f2
      case (None, None)         => None
    }

    def zero: Option[A] = None
  }

  implicit def optionEqual[A: Equal]: Equal[Option[A]] = new Equal[Option[A]] {
    def equal(o1: Option[A], o2: Option[A]): Boolean = (o1, o2) match {
      case (Some(a1), Some(a2)) => Equal[A].equal(a1, a2)
      case (None, None)         => false
      case (None, Some(_))      => false
      case (Some(_), None)      => false
    }
  }

  implicit def optionShow[A: Show]: Show[Option[A]] = new Show[Option[A]] {
    def show(o1: Option[A]): List[Char] = (o1 match {
      case Some(a1) => "Some(" + Show[A].show(a1) + ")"
      case None     => "None"
    }).toList
  }

  import Tags.{First, Last}

  implicit def optionFirst[A] = new Monoid[Option[A] @@ First] {
    def zero: Option[A] @@ First = Tag(None)

    def append(f1: Option[A] @@ First, f2: => Option[A] @@ First) = Tag(f1.orElse(f2))
  }

  implicit def optionLast[A] = new Monoid[Option[A] @@ Last] {
    def zero: Option[A] @@ Last = Tag(None)

    def append(f1: Option[A] @@ Last, f2: => Option[A] @@ Last) = Tag(f2.orElse(f1))
  }
}

trait OptionFunctions {
  final def some[A](a: A): Option[A] = Some(a)

  final def none[A]: Option[A] = None


  /**
   * Catamorphism over the option. Returns the provided function `some` applied to item contained in the Option
   * if it is defined, otherwise, the provided value `none`.
   */
  final def cata[A, X](oa: Option[A])(some: A => X, none: => X): X = oa match {
    case None    => none
    case Some(a) => some(a)
  }

  /**Alias for `cata` */
  final def fold[A, X](oa: Option[A])(some: A => X, none: => X): X = cata(oa)(some, none)

  final def toSuccess[A, E](oa: Option[A])(e: => E): Validation[E, A] = oa match {
    case Some(a) => Success(a)
    case None    => Failure(e)
  }

  final def toFailure[A, B](oa: Option[A])(b: => B): Validation[A, B] = oa match {
    case Some(e) => Failure(e)
    case None    => Success(b)
  }

  /**
   * Returns the item contained in the Option wrapped in type M if the Option is defined,
   * otherwise, the empty value for type M.
   */
  final def orEmpty[A, M[_] : Pointed : Empty](oa: Option[A]): M[A] = oa match {
    case Some(a) => Pointed[M].point(a)
    case None    => Empty[M].empty
  }

  /**
   * Returns the given value if None, otherwise lifts the Some value and passes it to the given function.
   */
  final def foldLift[F[_], A, B](oa: Option[A])(b: => B, k: F[A] => B)(implicit p: Pointed[F]): B = oa match {
    case None    => b
    case Some(a) => k(Pointed[F].point(a))
  }

  /**
   * Returns the given value if None, otherwise lifts the Some value to Option and passes it to the given function.
   */
  final def foldLiftOpt[A, B](oa: Option[A])(b: => B, k: Option[A] => B): B = {
    import scalaz.std.option.optionInstance
    foldLift[Option, A, B](oa)(b, k)
  }
}

object option extends OptionInstances with OptionFunctions {
  object optionSyntax extends scalaz.syntax.std.ToOptionV
}

