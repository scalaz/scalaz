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
   * A validator for converting a list of characters to an Int with the potential for a NumberFormatException.
   */
  val parseInt = kleisli[PartialType[Validation, NumberFormatException]#Apply](
    (s: List[Char]) => try { success(s.mkString.toInt) } catch { case e: NumberFormatException => fail(e) })

  /**
   * A validator for converting a list of characters to an Int.
   */
  val readInt = parseInt.compose[Option](_.right.toOption)                 

  /**
   * A validator for ensuring a list is not empty.
   */
  def notEmpty[A] = kleisli[Option]((cs: List[A]) => (cs: Option[NonEmptyList[A]]))

  /**
   * A validator for ensuring a character is a valid digit.
   */
  val isDigit = kleisli[Option]((c: Char) => charDigit(c))

  /**
   * A validator for ensuring a list of characters are all digits.
   */
  val isDigits = kleisli[Option]((ds: List[Char]) => isDigit.traverses(ds))

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

  /**
   * A validator for a specific length of a folding structure.
   */
  def lengthEqual[F[_]](n: Int)(implicit f: FoldLeft[F]) = lengthRange[F](n, n)

  /**
   * A validator for lists of length 1.
   */
  def length1[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a) => Some(a)
    case Nil => None
  })

  /**
   * A validator for lists of length 2.
   */
  def length2[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2) => Some(a1, a2)
    case Nil => None
  })

  /**
   * A validator for lists of length 3.
   */
  def length3[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3) => Some(a1, a2, a3)
    case Nil => None
  })

  /**
   * A validator for lists of length 4.
   */
  def length4[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4) => Some(a1, a2, a3, a4)
    case Nil => None
  })

  /**
   * A validator for lists of length 5.
   */
  def length5[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5) => Some(a1, a2, a3, a4, a5)
    case Nil => None
  })

  /**
   * A validator for lists of length 6.
   */
  def length6[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6) => Some(a1, a2, a3, a4, a5, a6)
    case Nil => None
  })

  /**
   * A validator for lists of length 7.
   */
  def length7[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7) => Some(a1, a2, a3, a4, a5, a6, a7)
    case Nil => None
  })

  /**
   * A validator for lists of length 8.
   */
  def length8[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8) => Some(a1, a2, a3, a4, a5, a6, a7, a8)
    case Nil => None
  })

  /**
   * A validator for lists of length 9.
   */
  def length9[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9)
    case Nil => None
  })

  /**
   * A validator for lists of length 10.
   */
  def length10[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    case Nil => None
  })

  /**
   * A validator for lists of length 11.
   */
  def length11[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    case Nil => None
  })

  /**
   * A validator for lists of length 12.
   */
  def length12[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    case Nil => None
  })

  /**
   * A validator for lists of length 13.
   */
  def length13[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    case Nil => None
  })

  /**
   * A validator for lists of length 14.
   */
  def length14[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    case Nil => None
  })

  /**
   * A validator for lists of length 15.
   */
  def length15[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
    case Nil => None
  })

  /**
   * A validator for lists of length 16.
   */
  def length16[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
    case Nil => None
  })

  /**
   * A validator for lists of length 17.
   */
  def length17[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
    case Nil => None
  })

  /**
   * A validator for lists of length 18.
   */
  def length18[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
    case Nil => None
  })

  /**
   * A validator for lists of length 19.
   */
  def length19[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
    case Nil => None
  })

  /**
   * A validator for lists of length 20.
   */
  def length20[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
    case Nil => None
  })

  /**
   * A validator for lists of length 21.
   */
  def length21[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
    case Nil => None
  })

  /**
   * A validator for lists of length 22.
   */
  def length22[A] = kleisli[Option]((as: List[A]) => as match {
    case List(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) => Some(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)
    case Nil => None
  })
}
