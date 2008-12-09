// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Applies one type argument of two. This is used for taking a type constructor of kind <code>* -> * -> *</code> to
 * <code>* -> *</code> by partially applying one type argument. The <code>Apply</code> type applies the first argument
 * and the <code>Flip</code> type applies the second argument. See also
 * <a href="https://lampsvn.epfl.ch/trac/scala/ticket/339">Ticket #339</a>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait PartialType[T[_, _], A] {
  type Apply[B] = T[A, B]

  type Flip[B] = T[B, A]
}
