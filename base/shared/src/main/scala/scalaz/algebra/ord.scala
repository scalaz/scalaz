package scalaz
package algebra

import scala.language.experimental.macros

import scala.{ Product, Serializable }

import core.EqClass

sealed abstract class Ordering extends Product with Serializable
final case object LT           extends Ordering
final case object GT           extends Ordering
final case object EQ           extends Ordering

trait OrdClass[A] extends EqClass[A] {
  def comp(a: A, b: A): Ordering

  def <(a: A, b: A): Boolean  = comp(a, b) eq LT
  def <=(a: A, b: A): Boolean = comp(a, b) ne GT
  def >(a: A, b: A): Boolean  = comp(a, b) eq GT
  def >=(a: A, b: A): Boolean = comp(a, b) ne LT
  def equal(a: A, b: A)       = comp(a, b) eq EQ
}

trait OrdSyntax {
  implicit final class ToOrdOps[A](a: A) {
    def comp(f: A)(implicit ev: Ord[A]): Ordering = macro meta.Ops.ia_1
  }
}
