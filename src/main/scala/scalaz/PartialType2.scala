// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision: 169 $
// $LastChangedDate: 2009-03-24 15:09:07 +1000 (Tue, 24 Mar 2009) $


package scalaz

/**
 * Applies one type argument of two where one of the type arguments has a kind * -> *.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision: 169 $<br>
 *          $LastChangedDate: 2009-03-24 15:09:07 +1000 (Tue, 24 Mar 2009) $<br>
 *          $LastChangedBy: tonymorris $
 */
trait PartialType2[T[_[_], _], A[_]] {
  type Apply[B] = T[A, B]
}
