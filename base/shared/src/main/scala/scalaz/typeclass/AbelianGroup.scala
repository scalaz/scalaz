package scalaz
package typeclass

/* An Abelian Group is a Group, such that
 * a1 |+| a2 = a2 |+| a1
 * */
trait AbelianGroup[A] {
  def group: Group[A]

}

object AbelianGroup extends AbelianGroupInstances {
  def apply[A](implicit A: AbelianGroup[A]): AbelianGroup[A] = A
}
