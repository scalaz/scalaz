package scalaz
package system

import scala.reflect.macros.whitebox.Context

trait Composer {
  def apply[O: c.WeakTypeTag](c: Context)(ts: Vector[c.Type], vs: Vector[c.Tree]): c.Expr[O]
  def typecheck(c: Context)(a: c.Type, b: c.Type): Either[String, c.Type]
  def isComposite(c: Context)(x: c.Type): Boolean
}

object Composer {
  def formatError(x: String, y: String): String = s"can't compose;\n    $x\n  with\n    $y"
  def formatErrorMessage(a: String, b: String)(msg: String) = {
    val error = formatError(a, b)
    s"$error\n  as\n    $msg"
  }

  import language.experimental.macros
  class Ops[XS, O] { final def compose: O = macro ComposerMacros.composeOps[XS, O] }
  object Ops { implicit def ops[XS, O]: Ops[XS, O] = macro ComposerMacros.materializeOps[XS, O] }
}
