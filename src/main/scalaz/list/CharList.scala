// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.list

/**
 * Functions over character lists.
 * 
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object CharList {

  implicit def CharListString(cs:List[Char]):String = cs.mkString

  implicit def CharListRichString(cs:List[Char]):runtime.RichString = CharListString(cs)
}
