// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.io

import java.io.FileInputStream

/**
 * Wraps <code>java.io.File</code> and provides additional methods.
 *
 * @see java.io.File
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait File {
  /**
   * The value of this file.
   */
  val file: java.io.File

  /**
   * The line separators of a text file (<code>\r</code> and <code>\n</code>).
   */
  val lineSeparators = List('\r'.toByte, '\n'.toByte)

  /**
   * Reads this file and applies the given function on each byte in the file, starting at the given value.
   *
   * @param x The first value to start applying the given function with.
   * @param f The function to apply on each byte in the file.
   */
  def read[X](x: X, f: (X, Byte) => X) = {
    val in = new FileInputStream(file)
    import javas.InputStream.InputStreamByteIterator

    try {
      in.foldLeft(x)(f)
    } finally {
      in.close
    }
  }

  /**
   * Executes the given side-effect for each byte in the file.
   */
  def each(f: Byte => Unit) = {
    val in = new FileInputStream(file)
    import javas.InputStream.InputStreamByteIterator

    try {
      in.foreach(f)
    } finally {
      in.close
    }
  }

  /**
   * Executes a function on each character in the file, then another function on the accumulated value for each line.
   *
   * @param x The first value to use in accumulating across the file.
   * @param f The function to apply for each character of the file, whose accumulated value is passed to the next
   * function for each line.
   * @param g The function to aply for each line in the file.
   */
  def readLines[X, Y](x: X, f: (X, Char) => X, y: Y, g: (Y, X) => Y) = {
    val in = new FileInputStream(file)
    import javas.InputStream.InputStreamByteIterator

    implicit def FL[X, Y](i: Iterator[Char]) = new {
      def foldFoldLeft[X, Y](p: Char => Boolean)(x: X, f: (X, Char) => X, y: Y, g: (Y, X) => Y): Y = {
        var t = x
        var u = y

        while(i.hasNext) {
          val c = i.next

          if(p(c)) {
            u = g(u, t)
            t = x
          } else
            t = f(t, c)
        }

        u
      }
    }

    try {
      in.map(_.toChar).foldFoldLeft(lineSeparators.contains(_))(x, f, y, g)
    } finally {
      in.close
    }
  }

  /**
   * Executes the given function for each line in the file.
   */
  def eachLine(f: Iterator[Char] => Unit) {
    import javas.InputStream.InputStreamByteIterator
    val in = new FileInputStream(file)

    try {
      while(in.hasNext) {
        val i = in.map(_.toChar).takeWhile(lineSeparators.contains(_))
        f(i)
        while(i.hasNext) i.next
      }
    } finally {
      in.close
    }
  }
}

/**
 * Functions over files.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object File {
  /**
   * Wraps a <code>java.io.File</code>.
   */
  implicit def JavaFileFile(f: java.io.File): File = new File {
    val file = f
  }

  /**
   * Unwraps a <code>java.io.File</code>.
   */
  implicit def FileJavaFile(f: File): java.io.File = f.file

  /**
   * Construct a file from the given string.
   */
  implicit def file(s: String): File = new java.io.File(s)

  /**
   * Construct a file from the given URI.
   */
  implicit def file(u: java.net.URI): File = new java.io.File(u)
}
