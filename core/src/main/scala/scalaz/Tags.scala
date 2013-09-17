package scalaz

import Tag.TagOf

/**
 * Type tags that are used to discriminate between alternative type class instances.
 *
 * @see [[scalaz.Tag]] and, `@@` in the package object [[scalaz]] .
 */
object Tags {
  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the first operand to append. */
  sealed trait FirstVal

  val FirstVal = new TagOf[FirstVal]

  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the last operand to append. */
  sealed trait LastVal

  val LastVal = new TagOf[LastVal]

  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the lesser of two operands. */
  sealed trait MinVal

  val MinVal = new TagOf[MinVal]

  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the greater of two operands. */
  sealed trait MaxVal

  val MaxVal = new TagOf[MaxVal]

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the first non-`zero` operand to append. */
  sealed trait First

  val First = new TagOf[First]

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the last non-`zero` operand to append. */
  sealed trait Last

  val Last = new TagOf[Last]

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the lesser of two operands, ignoring `zero`. */
  sealed trait Min

  val Min = new TagOf[Min]

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the greater of two operands, ignoring `zero`. */
  sealed trait Max

  val Max = new TagOf[Max]

  /** Type tag to choose a [[scalaz.Monoid]] instance for a numeric type that performs multiplication,
   *  rather than the default monoid for these types which by convention performs addition. */
  sealed trait Multiplication

  val Multiplication = new TagOf[Multiplication]

  /** Type tag to choose a [[scalaz.Monoid]] instance that inverts the operands to `append` before calling the
   *  natural [[scalaz.Monoid]] for the type.
   *
   *  Example:
   *  {{{
   *  import scalaz.{@@, Tag, Tags, Dual}
   *  import scalaz.std.string._
   *  import scalaz.syntax.monoid._
   *  import scalaz.Dual._
   *  Dual("World!") |+| Dual("Hello, ") // "Hello, World!"
   *  }}}
   */
  sealed trait Dual

  val Dual = new TagOf[Dual]

  /** Type tag to choose as [[scalaz.Applicative]] instance that performs zipping.
   *
   * @see [[scalaz.std.stream.streamZipApplicative]]
   */
  sealed trait Zip

  val Zip = new TagOf[Zip]

  /** Type tag to choose a [[scalaz.Monoid]] instance that performs disjunction (`||`) */
  sealed trait Disjunction

  val Disjunction = new TagOf[Disjunction]

  /** Type tag to choose a [[scalaz.Monoid]] instance that performs conjunction (`&&`) */
  sealed trait Conjunction

  val Conjunction = new TagOf[Conjunction]
}
