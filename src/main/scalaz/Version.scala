// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Prints the version of this software.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision<br>
 *          $LastChangedDate<br>
 *          $LastChangedBy$
 */
object Version extends Application {
  println("Scalaz version %build.number%")
  println("Compiled against Scala version %scala.version%")
  println("Copyright Tony Morris 2008-2009")
}
