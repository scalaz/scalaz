package scalaz

/**
 * Sub class to create a wrapper type for `X` as documentation that the sub class is a wrapper type
 * used in type class instances.
 * <p/>
 * The companion object provides an implicit conversion to unwrap `value`.
 *
 * @see scalaz.BooleanConjunction
 */
trait NewType[X] {
  val value: X
}

object NewType {
  implicit def NewTypeFrom[X](n: NewType[X]): X = n.value
}