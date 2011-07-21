package scalaz

sealed trait OptionW[A] extends PimpedType[Option[A]] {
  import Scalaz._

  /**
   * Catamorphism over the option. Returns the provided function `some` applied to item contained in the Option
   * if it is defined, otherwise, the provided value `none`.
   */
  def cata[X](some: A => X, none: => X): X = value match {
    case None => none
    case Some(a) => some(a)
  }

  /** Alias for `cata` */
  def fold[X](some: A => X, none: => X): X = cata(some, none)

  sealed trait Fold[X] {
    def none(s: => X): X
  }

  /**
   * Returns the provided function `s` applied to item contained in the Option if it is defined,
   * otherwise, the provided value `n`.
   * <p/>
   * This is a syntactic alternative to  { @link scalaz.OptionW # cata }
   * <p/>
   * Example:
   * <code>
   * option.some(_ * 2).none(0)
   * </code>
   */
  def some[X](s: A => X): Fold[X] = new Fold[X] {
    def none(n: => X): X = cata(s, n)
  }

  sealed trait Conditional[X] {
    def |(n: => X): X
  }

  /**
   * Ternary operator. Note that the arguments s and n are call-by-name.
   * <p/>
   * Example
   * <code>
   * option ? "defined" | "undefined"
   * </code>
   */
  def ?[X](s: => X): Conditional[X] = new Conditional[X] {
    def |(n: => X): X = value match {
      case None => n
      case Some(_) => s
    }
  }

  /**
   * Executes the provided side effect if the Option if it is undefined.
   */
  def ifNone(n: => Unit): Unit = if (value.isEmpty) n

  /**
   * Returns the item contained in the Option if it is defined, otherwise, raises an error with the provided message.
   */
  def err(message: => String): A = value getOrElse (sys.error(message))

  /**
   * Returns the item contained in the Option if it is defined, otherwise, the provided argument.
   */
  def |(a: => A): A = value getOrElse a

  /**
   * Returns the item contained in the Option if it is defined, otherwise, the zero element for the type A
   * <p/>
   * For example:
   * <pre>
   * val o: Option = None
   * val a: List[String] = ~o
   * </pre>
   */
  def unary_~(implicit z: Zero[A]): A = value getOrElse z.zero

  def orZero(implicit z: Zero[A]): A = ~this

  def toSuccess[E](e: => E): Validation[E, A] = value match {
    case Some(a) => Success(a)
    case None => Failure(e)
  }

  def toFailure[B](b: => B): Validation[A, B] = value match {
    case Some(e) => Failure(e)
    case None => Success(b)
  }

  def fst: FirstOption[A] = value

  def lst: LastOption[A] = value

  /**
   * Returns the item contained in the Option wrapped in type M if the Option is defined,
   * otherwise, the empty value for type M.
   */
  def orEmpty[M[_] : Pure : Empty]: M[A] = value match {
    case Some(a) => a η
    case None => <∅>
  }

  /**
   * Returns the given value if None, otherwise lifts the Some value and passes it to the given function.
   */
  def foldLift[F[_], B](b: => B, k: F[A] => B)(implicit p: Pure[F]): B = value match {
    case None    => b
    case Some(a) => k(a.η[F])
  }

  /**
   * Returns the given value if None, otherwise lifts the Some value to Option and passes it to the given function.
   */
  def foldLiftOpt[B](b: => B, k: Option[A] => B): B = foldLift[Option, B](b, k)

  /**
   * Returns a Done iteratee with the given value if the Option is not defined, otherwise runs the given function.
   */
  def iterDoneOr[B](b: => B, f: A => IterV[A, B]): IterV[A, B] = value match {
    case None    => IterV.Done(b, IterV.EOF.apply)
    case Some(a) => f(a)
  }
}

trait Options {
  implicit def OptionTo[A](o: Option[A]): OptionW[A] = new OptionW[A] {
    val value = o
  }

  def some[A](a: A): Option[A] = Some(a)

  def none[A]: Option[A] = None
}
