package scalaz

/**
 * Type tags that are used to discriminate between alternative type class instances.
 *
 * @see [[scalaz.Tag]] and, `@@` in the package object [[scalaz]] .
 */
object Tags {
  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the first operand to append. */
  sealed trait FirstVal

  def FirstVal[A](a: A): A @@ FirstVal = Tag[A, FirstVal](a)

  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the last operand to append. */
  sealed trait LastVal

  def LastVal[A](a: A): A @@ LastVal = Tag[A, LastVal](a)

  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the lesser of two operands. */
  sealed trait MinVal

  def MinVal[A](a: A): A @@ MinVal = Tag[A, MinVal](a)

  /** Type tag to choose a [[scalaz.Semigroup]] instance that selects the greater of two operands. */
  sealed trait MaxVal

  def MaxVal[A](a: A): A @@ MaxVal = Tag[A, MaxVal](a)

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the first non-`zero` operand to append. */
  sealed trait First

  def First[A](a: A): A @@ First = Tag[A, First](a)

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the last non-`zero` operand to append. */
  sealed trait Last

  def Last[A](a: A): A @@ Last = Tag[A, Last](a)

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the lesser of two operands, ignoring `zero`. */
  sealed trait Min

  def Min[A](a: A): A @@ Min = Tag[A, Min](a)

  /** Type tag to choose a [[scalaz.Monoid]] instance that selects the greater of two operands, ignoring `zero`. */
  sealed trait Max

  def Max[A](a: A): A @@ Max = Tag[A, Max](a)

  /** Type tag to choose a [[scalaz.Monoid]] instance for a numeric type that performs multiplication,
   *  rather than the default monoid for these types which by convention performs addition. */
  sealed trait Multiplication

  def Multiplication[A](a: A): A @@ Multiplication = Tag[A, Multiplication](a)

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

  /** Type tag to choose a [[scalaz.Monoid]] instance that performs conjunction (`&&`) */
  sealed trait Conjunction

  def Conjunction[A](a: A): A @@ Conjunction = Tag[A, Conjunction](a)
}
