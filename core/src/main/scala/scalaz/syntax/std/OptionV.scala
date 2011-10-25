package scalaz
package syntax
package std

import scalaz.std.Option
import scalaz.std.Option.{Last, First}

trait OptionV[A] extends SyntaxV[Option[A]] {
  def cata[X](some: A => X, none: => X): X = Option.cata(self)(some, none)
  def fold[X](oa: Option[A])(some: A => X, none: => X): X = cata(some, none)

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
    def |(n: => X): X = self match {
      case None => n
      case Some(_) => s
    }
  }


  /**
   * Executes the provided side effect if the Option if it is undefined.
   */
  def ifNone(n: => Unit): Unit = if (self.isEmpty) n

  /**
   * Returns the item contained in the Option if it is defined, otherwise, raises an error with the provided message.
   */
  def err(message: => String): A = self.getOrElse(sys.error(message))

  /**
   * Returns the item contained in the Option if it is defined, otherwise, the provided argument.
   */
  def |(a: => A): A = self getOrElse a

  /**
   * Returns the item contained in the Option if it is defined, otherwise, the zero element for the type A
   * <p/>
   * For example:
   * <pre>
   * val o: Option = None
   * val a: List[String] = ~o
   * </pre>
   */
  def unary_~(implicit z: Monoid[A]): A = self getOrElse z.zero
  
  def toSuccess[E](e: => E): Validation[E, A] = Option.toSuccess(self)(e) 
  
  def toFailure[B](b: => B): Validation[A, B] = Option.toFailure(self)(b)
  
  def first: Option[A] @@ First = Tag(self)
  
  def last: Option[A] @@ Last = Tag(self)
  
  def orEmpty[M[_] : Pointed : Empty]: M[A] = Option.orEmpty(self)
  
  def foldLift[F[_], B](b: => B, k: F[A] => B)(implicit p: Pointed[F]): B = Option.foldLift(self)(b, k)
  
  def foldLiftOpt[B](b: => B, k: Option[A] => B): B = Option.foldLiftOpt[A, B](self)(b, k)
}

trait ToOptionV {
  implicit def ToOptionVFromOption[A](a: Option[A]): OptionV[A] = new OptionV[A] {
    val self = a
  }
}
