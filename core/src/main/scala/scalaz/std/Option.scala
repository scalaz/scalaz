package scalaz
package std

trait OptionInstances0 {
  implicit def optionEqual[A](implicit A0: Equal[A]) = new OptionEqual[A] {
    implicit def A = A0
  }
}

trait OptionInstances extends OptionInstances0 {
  implicit val optionInstance = new Traverse[Option] with MonadPlus[Option] with Each[Option] with Index[Option] with Length[Option] with ApplicativePlus[Option] with Cozip[Option] {
    def point[A](a: => A) = Some(a)
    def each[A](fa: Option[A])(f: (A) => Unit) = fa foreach f
    def index[A](fa: Option[A], n: Int) = if (n == 0) fa else None
    def length[A](fa: Option[A]) = if (fa.isEmpty) 0 else 1
    override def ap[A, B](fa: => Option[A])(f: => Option[(A) => B]) = f match {
      case Some(f) => fa match {
        case Some(x) => Some(f(x))
        case None    => None
      }
      case None    => None
    }
    def bind[A, B](fa: Option[A])(f: A => Option[B]) = fa flatMap f
    override def map[A, B](fa: Option[A])(f: A => B) = fa map f
    def traverseImpl[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]) =
      fa map (a => F.map(f(a))(Some(_): Option[B])) getOrElse F.point(None)
    def empty[A]: Option[A] = None
    def plus[A](a: Option[A], b: => Option[A]) = a orElse b
    override def foldRight[A, B](fa: Option[A], z: => B)(f: (A, => B) => B) = fa match {
      case Some(a) => f(a, z)
      case None    => z
    }
    def cozip[A, B](a: Option[Either[A, B]]) =
      a match {
        case None => Left(None)
        case Some(e) => e match {
          case Left(a) => Left(Some(a))
          case Right(b) => Right(Some(b))
        }
      }
  }

  implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def append(f1: Option[A], f2: => Option[A]) = (f1, f2) match {
      case (Some(a1), Some(a2)) => Some(Semigroup[A].append(a1, a2))
      case (Some(a1), None)     => f1
      case (None, Some(a2))     => f2
      case (None, None)         => None
    }

    def zero: Option[A] = None
  }

  implicit def optionOrder[A](implicit A0: Order[A]): Order[Option[A]] = new OptionOrder[A] {
    implicit def A = A0
  }

  implicit def optionShow[A: Show]: Show[Option[A]] = new Show[Option[A]] {
    def show(o1: Option[A]) = o1 match {
      case Some(a1) => "Some(".toList ::: Show[A].show(a1) ::: ")".toList
      case None     => "None".toList
    }
  }

  import Tags.{First, Last}

  implicit def optionFirst[A] = new Monoid[Option[A] @@ First] {
    def zero: Option[A] @@ First = Tag(None)

    def append(f1: Option[A] @@ First, f2: => Option[A] @@ First) = Tag(f1.orElse(f2))
  }

  implicit def optionFirstShow[A: Show]: Show[Option[A] @@ First] = Tag.subst(Show[Option[A]])

  implicit def optionFirstOrder[A: Order]: Order[Option[A] @@ First] = Tag.subst(Order[Option[A]])

  implicit def optionFirstMonad[A]: Monad[({type f[x] = Option[x] @@ First})#f] = new Monad[({type f[x] = Option[x] @@ First})#f] {
    def point[A](a: => A): (Option[A] @@ Tags.First) = Tag(Some(a))
    override def map[A, B](fa: Option[A] @@ First)(f: (A) => B) = Tag(fa map f)
    def bind[A, B](fa: (Option[A] @@ Tags.First))(f: (A) => (Option[B] @@ Tags.First)): (Option[B] @@ Tags.First) = Tag(fa flatMap f)
  }


  implicit def optionLast[A] = new Monoid[Option[A] @@ Last] {
    def zero: Option[A] @@ Last = Tag(None)

    def append(f1: Option[A] @@ Last, f2: => Option[A] @@ Last) = Tag(f2.orElse(f1))
  }

  implicit def optionLastShow[A: Show]: Show[Option[A] @@ Last] = Tag.subst(Show[Option[A]])

  implicit def optionLastOrder[A: Order]: Order[Option[A] @@ Last] = Tag.subst(Order[Option[A]])

  implicit def optionLastMonad[A]: Monad[({type f[x] = Option[x] @@ Last})#f] = new Monad[({type f[x] = Option[x] @@ Last})#f] {
    def point[A](a: => A): (Option[A] @@ Tags.Last) = Tag(Some(a))
    override def map[A, B](fa: Option[A] @@ Last)(f: (A) => B) = Tag(fa map f)
    def bind[A, B](fa: (Option[A] @@ Tags.Last))(f: (A) => (Option[B] @@ Tags.Last)): (Option[B] @@ Tags.Last) = Tag(fa flatMap f)
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
  final def orEmpty[A, M[_] : Pointed : PlusEmpty](oa: Option[A]): M[A] = oa match {
    case Some(a) => Pointed[M].point(a)
    case None    => PlusEmpty[M].empty
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
  object optionSyntax extends scalaz.syntax.std.ToOptionV with scalaz.syntax.std.ToOptionIdV 
}

//
// Type class implementation traits
//

trait OptionEqual[A] extends Equal[Option[A]] {
  implicit def A: Equal[A]

  override def equalIsNatural: Boolean = A.equalIsNatural

  override def equal(o1: Option[A], o2: Option[A]): Boolean = (o1, o2) match {
    case (Some(a1), Some(a2)) => A.equal(a1, a2)
    case (None, None)         => true
    case (None, Some(_))      => false
    case (Some(_), None)      => false
  }
}


trait OptionOrder[A] extends Order[Option[A]] with OptionEqual[A] {
  implicit def A: Order[A]

  import Ordering._

  def order(f1: Option[A], f2: Option[A]) = (f1, f2) match {
    case (Some(a1), Some(a2)) => Order[A].order(a1, a2)
    case (None, Some(_))      => GT
    case (Some(_), None)      => LT
    case (None, None)         => EQ
  }
}
