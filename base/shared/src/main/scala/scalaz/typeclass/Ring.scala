package scalaz
package typeclass

/* An Ring consists in:
 * - An Abelian group, providing addition
 * - A Monoid, providing multiplication
 * */

trait Ring[A] {
  def abelianGroup: AbelianGroup[A]
  def monoid:Monoid[A]
  def addition(a1:A, a2 : => A):A = abelianGroup.group.monoid.semigroup.append(a1,a2)
  def multiplication(a1:A, a2: => A):A = monoid.semigroup.append(a1, a2)

}

object Ring extends RingInstances {
  def apply[A](implicit A: Ring[A]): Ring[A] = A
}
