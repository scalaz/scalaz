package scalaz

/**
 * Type tags that are used to discriminate between alternative type class instances.
 *
 * @see [[scalaz.Tag]] and, `@@` in the package object [[scalaz]] .
 */
object Tags {

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the first operand to append */
  sealed trait First

  def First[A](a: A): A @@ First = Tag[A, First](a)

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the last operand to append */
  sealed trait Last

  def Last[A](a: A): A @@ Last = Tag[A, Last](a)

  /** Type tag to choose a [[scalaz.Monoid]] instance for a numeric type that performs multiplication,
   *  rather than the default monoid for these types which by convention performs addition. */
  sealed trait Multiplication

  def Multiplication[A](a: A): A @@ Multiplication = Tag[A, Multiplication](a)

  /** Type tag to choose a [[scalaz.Monoid]] instance that inverts the operands to `append` before calling the
   *  natural [[scalaz.Monoid]] for the type.
   *
   *  Example:
   *  {{{
   *  import std.string._
   *  Monoid[Dual[String]].append("World!", "Hello, ") // "Hello, World!"
   *  }}}
   */
  sealed trait Dual

  def Dual[A](a: A): A @@ Dual = Tag[A, Dual](a)

  /** Type tag to choose as [[scalaz.Applicative]] instance that performs zipping.
   *
   * @see [[scalaz.std.stream.streamZipApplicative]]
   */
  sealed trait Zip

  def Zip[A](a: A): A @@ Zip = Tag[A, Zip](a)

  /** Type tag to choose a [[scalaz.Monoid]] instance that performs disjunction (`||`) */
  sealed trait Disjunction

  def Disjunction[A](a: A): A @@ Disjunction = Tag[A, Disjunction](a)

  /** Type tag to choose a [[scalaz.Monoid]] instance that performs conjunction (`||`) */
  sealed trait Conjunction

  def Conjunction[A](a: A): A @@ Conjunction = Tag[A, Conjunction](a)
}
