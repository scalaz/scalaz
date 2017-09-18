package scalaz
package meta

import scala.reflect.macros.whitebox.Context

// Originally inspired by http://typelevel.org/blog/2013/10/13/spires-ops-macros.html

class Ops(val c: Context) {
  import c.universe._

  def _f0[R]: Expr[R] = {
    val (ev, lhs) = unpack
    c.Expr[R](Apply(Select(ev, TermName(c.macroApplication.symbol.name.toString)), List(lhs)))
  }

  def _f1[A, R](f: Expr[A]): Expr[R] = {
    val (ev, lhs) = unpack
    c.Expr[R](Apply(Apply(Select(ev, TermName(c.macroApplication.symbol.name.toString)), List(lhs)), List(f.tree)))
  }

  def _f2[A, B, R](f: Expr[A])(g: Expr[B]): Expr[R] = {
    val (ev, lhs) = unpack
    val y = Apply(Select(ev, TermName(c.macroApplication.symbol.name.toString)), List(lhs))
    val toappl = Apply(y, List(f.tree))
    c.Expr[R](Apply(toappl, List(g.tree)))
  }

  def unpack = {
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(x)), List(ev)) => (ev, x)
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }

}
