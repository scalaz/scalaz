// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

import validation.Validation

/**
 * Wraps <code>scala.Option</code> and provides additional methods.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait OptionW[A] {
  /**
   * The value of this option.
   */
  val option: Option[A]

  /**
   * The catamorphism for option.
   */
  def fold[X](none: => X, some: A => X) = option match {
    case None => none
    case Some(a) => some(a)
  }

  /**
   * Returns the first argument if this is <code>None</code>, otherwise the second argument.
   *
   * @param none The value to return if this is <code>None</code>.
   * @param some The value to return if this is <code>Some</code>.
   */
  def ?[X](none: => X, some: => X) = fold(none, a => some)

  /**
   * Perform the given side-effect if this is <code>None</code>.
   */
  def ifNone(n: => Unit) = if(option.isEmpty) n

  /**
   * Returns the value in this <code>Some</code>, otherwise throws an error with the given message.
   */
  def err(message: => String) = option getOrElse (error(message))

  /**
   * A synonym for <code>Option.getOrElse</code>.
   */
  def |[AA >: A](a: => AA) = option getOrElse a

  /**
   * Returns <code>null</code> if this is <code>None</code>, otherwise the value from this <code>Some</code>.
   */
  def toNull = option getOrElse null.asInstanceOf[A]

  /**
   * Returns a success with the given value if this is <code>None</code>, otherwise returns this value in failure.
   */
  def toFail[B](success: => B): Validation[A, B] = option.toLeft(success)

  /**
   * Returns a failure with the given value if this is <code>None</code>, otherwise returns this value in success.
   */
  def toSuccess[B](fail: => B): Validation[B, A] = option.toRight(fail)

  /**
   * A synonym for <code>toSuccess</code>.
   */
  def ~[B](fail: => B): Validation[B, A] = toSuccess(fail)

  /**
   * Maps the given function then <code>getOrElse</code> the given value.
   */
  def >|[B](f: A => B, v: => B) = option map f getOrElse v

  /**
   * flatMaps the given function then <code>getOrElse</code> the given value.
   */
  def >>=|[B](f: A => Option[B], v: => B) = option flatMap f getOrElse v

  /**
   * Returns an xml text element with the show implementation or an empty string if there is no value in this option.
   */
  def unary_-(implicit s: Show[A]) = xml.Text(option map(Show.shows(_)) getOrElse "")

  /**
   * Returns the element value in this option or the zero for the element type.
   */
  def unary_~(implicit z: control.Zero[A]) = option getOrElse z.zero
}

import control.{FoldRight, Empty, Plus, Pure}

/**
 * Functions over option values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object OptionW {
  /**
   * Unwraps a <code>scala.Option</code>.
   */
  implicit def OptionWOption[A](o: OptionW[A]): Option[A] = o.option

  /**
   * Wraps a <code>scala.Option</code>.
   */
  implicit def OptionOptionW[A](o: Option[A]): OptionW[A] = new OptionW[A] {
    val option = o
  }

  /**
   * Returns a <code>Some</code> with the given argument.
   */
  def some[A](a: A): Option[A] = Some(a)

  /**
   * Returns a <code>None</code> value.
   */
  def none[A]: Option[A] = None

  /**
   * Returns <code>None</code> if the given argument is <code>null</code>, otherwise the given argument in
   * <code>Some</code>.
   */
  implicit def onull[A](a: A) = if(a == null) None else Some(a)

  /**
   * Returns all the <code>Some</code> values in the given container of option.
   */
  def somesT[A, FD[_], MP[_]](os: FD[Option[A]])(implicit fd: FoldRight[FD], e: Empty[MP], p: Plus[MP], pr: Pure[MP]): MP[A] =
    fd.foldRight[Option[A], MP[A]](os, e.empty, (a, b) => a match {
      case Some(a) => p.plus(pr.pure(a), b)
      case None => b
    })

  /**
   * Returns all the <code>Some</code> values in the given container of option.
   */
  def somes[A](os: List[Option[A]]) = somesT[A, List, List](os)

  /**
   * The monad join operator, projecting the given argument to one level out. Equivalent to
   * <code>o.flatMap(identity)</code>.
   */
  def join[A](o: Option[Option[A]]) = o.flatMap(x => x)

  /**
   * If the given condition is <code>true</code>, return the given value in <code>Some</code>, otherwise return
   * <code>None</code>.
   *
   * @param c The condition to test and if <code>true</code> returns the given value in <code>Some</code>, otherwise
   * returns <code>None</code>.
   * @param a The value to return in <code>Some</code> if the given condition is <code>true</code>. 
   */
  def cond[A](c: Boolean, a: => A) = if(c) Some(a) else None
}
