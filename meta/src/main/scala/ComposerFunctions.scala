package scalaz
package meta

import scala.reflect.macros.whitebox.Context

object ComposerFunctions extends Composer {
  import MacroUtils._

  def apply[O: c.WeakTypeTag](c: Context)(ts: Vector[c.Type], vs: Vector[c.Tree]): c.Expr[O] = {
    import c.universe._
    val (a, b) = funapply(c)(weakTypeOf[O])
    val body = (1 until vs.size).foldLeft(q"${vs(0)}(a)") {
      case (exp, i) => q"${vs(i)}($exp)"
    }
    c.Expr[O](q"new Function1[$a, $b] { def apply(a: $a): $b = $body }")
  }

  def typecheck(c: Context)(a: c.Type, b: c.Type): Either[String, c.Type] = {
    import c.universe._
    import Composer.{formatError, formatErrorMessage}

    def formatError0(a: c.Type, b: c.Type)(s: c.Type, t: c.Type, u: c.Type, v: c.Type): String = {
      formatErrorMessage(s"$s => ${highlight(t.toString)}", s"${highlight(u.toString)} => $v")(s"$t is not a $u")
    }

    def formatError1(x: c.Type)(a: c.Type, b: c.Type): String = {
      def f(y: c.Type) = if (y == x) highlight(y.toString) else y.toString
      formatErrorMessage(f(a), f(b))(s"$x is not a function")
    }

    List(a, b).filterNot(_ <:< typeOf[_ => _]).headOption match {
      case None     =>
        val (s, t) = funapply(c)(a)
        val (u, v) = funapply(c)(b)
        if (t <:< u) Right(fapply(c)(s, v))
        else Left(formatError0(a, b)(s, t, u, v))
      case Some(x)  => Left(formatError1(x)(a, b))
    }
  }

  def isComposite(c: Context)(x: c.Type): Boolean = {
    import c.universe._
    x <:< typeOf[_ => _]
  }
}
