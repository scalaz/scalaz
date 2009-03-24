// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Applies one type argument of two where one of the type arguments has a kind * -> *.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait PartialType2[T[_[_], _], A[_]] {
  type Apply[B] = T[A, B]
}
