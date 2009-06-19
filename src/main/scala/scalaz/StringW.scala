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
  import Scalaz._

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
  
  def nelErr(message: => String) = nel(error(message))
  
  def unsafeNel = nelErr("cannot turn empty string into NonEmptyList")

  import java.io.FileInputStream

  def readFile[X](x: X, f: (X, Byte) => X) = {
    val in = new FileInputStream(s)

    try {
      in.elements.foldLeft(x)(f)
    } finally {
      in.close
    }
  }

  def fileEach(f: Byte => Unit) {
    val in = new FileInputStream(s)

    try {
      in.elements.foreach(f)
    } finally {
      in.close
    }
  }

  def readLines[X, Y](x: X, f: (X, Char) => X, y: Y, g: (Y, X) => Y) {
    val in = new FileInputStream(s)

    try {
      var t = x
      var u = y
      val i = in.elements
      val lineSeparators = List('\r'.toByte, '\n'.toByte)

      while(i.hasNext) {
        val c = i.next.toChar

        if(lineSeparators.contains(c)) {
          u = g(u, t)
          t = x
        } else
          t = f(t, c)
      }

      u
    } finally {
      in.close
    }
  }
}

object StringW {
  implicit def StringTo(ss: String): StringW = new StringW {
    val s = ss
  }

  implicit def StringFrom(s: StringW) = s.s
}
