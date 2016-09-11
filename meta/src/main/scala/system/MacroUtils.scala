package scalaz
package system

import scala.reflect.macros.whitebox.Context

object MacroUtils {
  def fapply(c: Context)(a: c.Type, b: c.Type): c.Type = {
    import c.universe._
    appliedType(definitions.FunctionClass(1), List(a, b))
  }

  def funapply(c: Context)(t: c.Type): (c.Type, c.Type) = {
    import c.universe._
    val TypeRef(_, _, a :: b :: Nil) = t
    (a, b)
  }

  def highlight(msg: String): String = highlightWith(msg, Console.RED)
  def highlightWith(msg: String, color: String): String = (color + msg) ++ Console.RESET
}
