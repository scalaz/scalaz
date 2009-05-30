package scalaz

sealed trait StringW {
  /**
   * The value of this string.
   */
  val s: String

  /**
   * Returns the same String value if the given value is 1 otherwise pluralises this String by appending an "s" unless
   * this String ends with "y" and not one of ["ay", "ey", "iy", "oy", "uy"] in which case the 'y' character is chopped and "ies"
   * is appended.
   */
  def plural(n: Long) = if(n == 1L) s else
                       if((s endsWith "y") && (List("ay", "ey","iy", "oy", "uy") forall (!s.endsWith(_)))) s.take(s.length - 1) + "ies"
                       else s + "s"

  import xml._
  import Identity._

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def node[A](prefix: String, attributes: MetaData, scope: NamespaceBinding, a: Option[A])(implicit sh: Show[A]) =
    a match {
      case Some(t) => Elem(prefix, s, Null, TopScope, t.text)
      case None => Text("")
    }

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def node[A](prefix: String, a: Option[A])(implicit sh: Show[A]): Node =
    node(prefix, Null, TopScope, a)

  /**
   * Construct an XML node based on the given option value. If there is no value available, then an empty text node is returned,
   * otherwise, the string representation (using show) of the value is returned in an element with the given label.
   */
  def |:|[A](a: Option[A])(implicit sh: Show[A]): Node =
    node(null, a)

  def encode(implicit c: CharSet) = s getBytes c.value
  

  /**
   * Constructs a non-empty list with the value if it is not empty, otherwise, throws an error.
   */
  def nel : Option[NonEmptyList[Char]] = if (s.length == 0) None else {
    val l = s.toList
    Some(NonEmptyList.nel(l.head, l.tail))
  }
  
  /**
   * Constructs a non-empty list with the given string if it is not empty, otherwise, returns the second argument.
   */
  def nel(e: => NonEmptyList[Char]) : NonEmptyList[Char] = nel getOrElse e
}

object StringW {
  implicit def StringTo(ss: String): StringW = new StringW {
    val s = ss
  }

  implicit def StringFrom(s: StringW) = s.s
}
