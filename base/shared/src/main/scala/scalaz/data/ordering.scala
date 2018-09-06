package scalaz
package data

import scala.{ Product, Serializable }

import tc._

sealed abstract class Ordering extends Product with Serializable
final case object LT           extends Ordering
final case object GT           extends Ordering
final case object EQ           extends Ordering

object Ordering {
  implicit val orderingEq: Eq[Ordering] = Eq.byReference[Ordering]
  implicit val orderingDebug: Debug[Ordering] = Debug.fromToString[Ordering]
}
