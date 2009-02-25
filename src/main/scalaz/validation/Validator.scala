package scalaz.validation

/**
 * Validators using the kleisli structure.
 *
 * @see Kleisli
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Validator {
  import control.Kleisli
  import control.Kleisli._
  import control.FoldLeft
  import control.FoldLeftW._
  import list.NonEmptyList

  import Validation.{success, fail}
  import scalaz.Digit.charDigit
  
  /**
   * A validator for converting a String to an Int with the potential for a NumberFormatException.
   */
  val parseInt = kleisli[PartialType[Validation, NumberFormatException]#Apply](
    (s: String) => try { success(s.toInt) } catch { case e: NumberFormatException => fail(e) })

  /**
   * A validator for ensuring a list is not empty.
   */
  def notEmpty[A] = kleisli[Option]((cs: List[A]) => (cs: Option[NonEmptyList[A]]))

  /**
   * A validator for ensuring a character is a valid digit.
   */
  def isDigit = kleisli[Option]((c: Char) => charDigit(c))

  /**
   * A validator for ensuring a list of characters are all digits.
   */
  def isDigits = kleisli[Option]((ds: List[Char]) => isDigit.traverses(ds))

  /**
   * A validator for a minimum value of an integer.
   */
  def intMin(min: Int) = filter((n: Int) => n >= min)

  /**
   * A validator for a maximum value of an integer.
   */
  def intMax(max: Int) = filter((n: Int) => n <= max)

  /**
   * A validator for ensuring an int value is within a range.
   */
  def intRange(min: Int, max: Int) = intMin(min) >=> intMax(max)

  /**
   * A validator for a minimum length of a folding structure.
   */
  def lengthMin[F[_]](min: Int)(implicit f: FoldLeft[F]) = intMin(min) comap ((x: F[_]) => {
    implicit val k = foldleft[F](x)
    k.items
  })

  /**
   * A validator for a maximum length of a folding structure.
   */
  def lengthMax[F[_]](max: Int)(implicit f: FoldLeft[F]) = intMax(max) comap ((x: F[_]) => {
    implicit val k = foldleft[F](x)
    k.items
  })

  /**
   * A validator for a range of the length of a folding structure.
   */
  def lengthRange[F[_]](min: Int, max: Int)(implicit f: FoldLeft[F]) = intRange(min, max) comap ((x: F[_]) => {
    implicit val k = foldleft[F](x)
    k.items
  })
}
