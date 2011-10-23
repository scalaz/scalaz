package scalaz
package effect

private[effect] case class World[A]()

sealed trait RealWorld

object RealWorld extends RealWorlds

trait RealWorlds {
  private[effect] val realWorld = World[RealWorld]()
}
