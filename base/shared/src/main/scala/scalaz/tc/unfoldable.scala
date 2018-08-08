package scalaz
package tc

import scalaz.data.{ IList, Maybe, Maybe2 }

trait UnfoldableClass[T[_]] {
  def unfoldRight[A, B](f: B => Maybe2[A, B])(b: B): Maybe[T[A]]
  def fromList[A](as: IList[A]): Maybe[T[A]] = unfoldRight(IList.uncons[A])(as)
}
