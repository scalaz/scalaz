package scalaz
package plugin

import scala.tools.nsc._
import scala.tools.nsc.plugins._

class ScalazPlugin(val global: Global) extends Plugin { plugin =>
  val name        = "scalaz"
  val description = "scalaz"

  val scalazDefns = new {
    val global: plugin.global.type = plugin.global
  } with Definitions

  scalazDefns.init()

  object sufficiency extends {
    val global: plugin.global.type                                  = plugin.global
    val scalazDefns: Definitions { val global: plugin.global.type } = plugin.scalazDefns
  } with SufficiencyChecker

  val components = List(
    "suff" -> sufficiency,
  ).flatMap {
    case (opt, phf) =>
      if (options.contains(s"-$opt")) None
      else Some(phf)
  }
}
