package scalaz
package wrap

sealed trait OptionW[A] {
  val value: Option[A]

  import *._
  import Validation._
  import newtypes.{FirstOption, LastOption}
  import iteratee.IterateeT, IterateeT._
  import iteratee.Input._

  /**
   * Catamorphism over the option. Returns the provided function `some` applied to item contained in the Option
   * if it is defined, otherwise, the provided value `none`.
   */
  def cata[X](some: A => X, none: => X): X = value match {
    case None => none
    case Some(a) => some(a)
  }

  /**Alias for `cata` */
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

  def toSuccess[E](e: => E): Validation[E, A] = value match {
    case Some(a) => success(a)
    case None => failure(e)
  }

  def toFailure[B](b: => B): Validation[A, B] = value match {
    case Some(e) => failure(e)
    case None => success(b)
  }

  def first: FirstOption[A] =
    value.*-->[FirstOption[A]]

  def last: LastOption[A] =
    value.*-->[LastOption[A]]

  /**
   * Returns the item contained in the Option wrapped in type M if the Option is defined,
   * otherwise, the empty value for type M.
   */
  def orEmpty[M[_] : Pointed : Empty]: M[A] = value match {
    case Some(a) => implicitly[Pointed[M]].point(a)
    case None => implicitly[Empty[M]].empty
  }

  /**
   * Returns the given value if None, otherwise lifts the Some value and passes it to the given function.
   */
  def foldLift[F[_], B](b: => B, k: F[A] => B)(implicit p: Pointed[F]): B = value match {
    case None => b
    case Some(a) => k(implicitly[Pointed[F]].point(a))
  }

  /**
   * Returns the given value if None, otherwise lifts the Some value to Option and passes it to the given function.
   */
  def foldLiftOpt[B](b: => B, k: Option[A] => B): B = foldLift[Option, B](b, k)

  /**
   * Returns a Done iteratee with the given value if the Option is not defined, otherwise runs the given function.
   */
  def doneOr[F[_], B](b: => B, f: A => IterateeT[A, F, B]): IterateeT[A, F, B] = value match {
    case None => doneT(b, eofInput)
    case Some(a) => f(a)
  }
}

object OptionW extends OptionWs

trait OptionWs {
  implicit def OptionTo[A](o: Option[A]): OptionW[A] = new OptionW[A] {
    val value = o
  }

  def some[A](a: A): Option[A] = Some(a)

  def none[A]: Option[A] = None
}
