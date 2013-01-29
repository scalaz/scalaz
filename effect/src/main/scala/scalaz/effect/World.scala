package scalaz
package effect

private[effect] case class Tower[A]()

sealed trait IvoryTower

object IvoryTower extends IvoryTowers

trait IvoryTowers {
  private[effect] val ivoryTower = Tower[IvoryTower]()
}
