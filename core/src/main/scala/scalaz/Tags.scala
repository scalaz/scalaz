package scalaz

/**
 * Type tags that are used to discriminate between alternative type class instances.
 *
 * @see [[scalaz.Tag]] and, `@@` in the package object [[scalaz]] .
 */
object Tags {
  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the first operand to append. */
  sealed trait FirstVal

  val FirstVal = Tag.of[FirstVal]

  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the last operand to append. */
  sealed trait LastVal

  val LastVal = Tag.of[LastVal]

  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the lesser of two operands. */
  sealed trait MinVal

  val MinVal = Tag.of[MinVal]

  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the greater of two operands. */
  sealed trait MaxVal

  val MaxVal = Tag.of[MaxVal]

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the first non-`zero` operand to append. */
  sealed trait First

  val First = Tag.of[First]

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the last non-`zero` operand to append. */
  sealed trait Last

  val Last = Tag.of[Last]

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the lesser of two operands, ignoring `zero`. */
  sealed trait Min

  val Min = Tag.of[Min]

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the greater of two operands, ignoring `zero`. */
  sealed trait Max

  val Max = Tag.of[Max]

  /** Type tag to choose a [[scalaz.Monoid]] instance for a numeric type that performs multiplication,
   *  rather than the default monoid for these types which by convention performs addition. */
  sealed trait Multiplication

  val Multiplication = Tag.of[Multiplication]

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

  val Dual = Tag.of[Dual]

  /** Type tag to choose as [[scalaz.Applicative]] instance that performs zipping.
   *
   * @see [[scalaz.std.stream.streamZipApplicative]]
   */
  sealed trait Zip

  val Zip = Tag.of[Zip]

  /** Type tag to choose a [[scalaz.Monoid]] instance that performs disjunction (`||`) */
  sealed trait Disjunction

  val Disjunction = Tag.of[Disjunction]

  /** Type tag to choose a [[scalaz.Monoid]] instance that performs conjunction (`&&`) */
  sealed trait Conjunction

  val Conjunction = Tag.of[Conjunction]

  /** Type tag to choose a [[scalaz.Applicative]] instance that runs scalaz.concurrent.Futures in parallel. */
  sealed trait Parallel

  val Parallel = Tag.of[Parallel]

}
