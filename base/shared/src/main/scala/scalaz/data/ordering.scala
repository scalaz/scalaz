package scalaz
package data

import scala.{ Product, Serializable }

sealed abstract class Ordering extends Product with Serializable
final case object LT           extends Ordering
final case object GT           extends Ordering
final case object EQ           extends Ordering
