package scalaz
package syntax

/** Provides [[Const]]-related methods for any value. */
final class ConstOps[A](val self: A) extends AnyVal {
  /** Wrap `self` in a [[Const]].
    *
    * Can have better type inference than the [[Const.apply]] constructor.
    */
  def const[B]: Const[A, B] = Const(self)
}

trait ToConstOps {
  implicit def ToConstOps[A](a: A): ConstOps[A] = new ConstOps(a)
}
