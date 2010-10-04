package scalaz

/**
 * Sub class to create a wrapper type for `X` as documentation that the sub class follows the
 * 'pimp my library' pattern. http://www.artima.com/weblogs/viewpost.jsp?thread=179766
 * <p/>
 * The companion object provides an implicit conversion to unwrap `value`.
 *
 * @see scalaz.BooleanW
 */
trait PimpedType[X] {
  val value: X
}

object PimpedType {
  implicit def UnwrapPimpedType[X](p: PimpedType[X]): X = p.value
}