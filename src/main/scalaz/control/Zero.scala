package scalaz.control

/**
 * The zero/identity element for a set.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Zero[Z] {
  /**
   * The zero/identity element for a set.
   */
  val zero: Z
}

/**
 * Functions over zero values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Zero {
  /**
   * Construct a zero from the given value.
   */
  def z[Z](z: Z) = new Zero[Z] {
    val zero = z
  }

  /**
   * A zero for <code>Predef.String</code>.
   */
  implicit val StringZero = z("")

  /**
   * A zero for integer addition.
   */
  implicit val IntAdditionZero = z(0)

  /**
   * A zero for integer multiplication.
   */
  implicit val IntMultiplicationZero = z(1)

  /**
   * A zero for boolean disjunction.
   */
  implicit val BooleanDisjunctionZero = z(false)

  /**
   * A zero for boolean conjunction.
   */
  implicit val BooleanConjunctionZero = z(true)

  /**
   * A zero for <code>forall T. scala.Option[T]</code>.
   */
  implicit def OptionZero[A] = z[Option[A]](None)

  /**
   * A zero for <code>forall T. scala.List[T]</code>.
   */
  implicit def ListZero[A] = z[List[A]](Nil)

  /**
   * A zero for <code>forall T. scala.Stream[T]</code>.
   */
  implicit def StreamZero[A] = z[Stream[A]](Stream.empty)

  /**
   * A zero for <code>forall T. scala.Array[T]</code>.
   */
  implicit def ArrayZero[A] = z(new Array[A](0))

  /**
   * A zero for <code>forall T U. scala.Function1[T, U]</code>.
   */
  implicit def Function1Zero[A, B](implicit zz: Zero[B]) = z((a: A) => zz.zero)

  /**
   * A zero for <code>forall T U. scala.Either[T, U]</code>.
   */
  implicit def EitherZero[A, B](implicit zz: Zero[A]) = z[Either[A, B]](Left(zz.zero))

  /**
   * A zero for <code>forall T U. scala.Either[U, T]</code>.
   */
  implicit def FlipEitherZero[A, B](implicit zz: Zero[A]) = z[Either[B, A]](Right(zz.zero))

  /**
   * A zero for <code>forall T U. scala.Either.LeftProjection[U, T]</code>.
   */
  implicit def EitherLeftZero[A, B](implicit zz: Zero[A]) = z[Either.LeftProjection[A, B]](Left(zz.zero).left)

  /**
   * A zero for <code>forall T U. scala.Either.RightProjection[T, U]</code>.
   */
  implicit def EitherRightZero[A, B](implicit zz: Zero[A]) = z[Either.RightProjection[B, A]](Right(zz.zero).right)

  /**
   * A zero for <code>forall K V. scala.collection.immutable.Map[K, V]</code>.
   */
  implicit def MapZero[K, V] = z(Map.empty[K, V])

  import xml.{Text, NodeSeq}

  /**
   * A zero for XML node sequences - an empty element.
   */
  implicit def NodeSeqZero = z[NodeSeq](Text(""))

  /**
   *  A zero for <code>forall M. scalaz.control.Pure[M]</code>.
   */
  def PureZero[M[_]](implicit m: Monad[M]) = z[Pure[M]](m)

  /**
   * Return the zero value for the given implementation.
   */
  def zero[A](implicit z: Zero[A]) = z.zero
}
