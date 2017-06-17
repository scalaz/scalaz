package scalaz
package meta

import scala.reflect.macros.whitebox.Context

// Originally inspired by http://typelevel.org/blog/2013/10/13/spires-ops-macros.html

object Ops {
  
  
  def _f0[R](c: Context): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, TermName(c.macroApplication.symbol.name.toString)), List(lhs)))
  }

  def _f1[A, R](c: Context)(f: c.Expr[A]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Apply(Select(ev, TermName(c.macroApplication.symbol.name.toString)), List(lhs)), List(f.tree)))
  }

  def _f1t[A, R, T](c: Context)(f: c.Expr[A])(implicit tTag: c.WeakTypeTag[T]): c.Expr[R] = {
    import c.universe._
    val methodNameEnd = tTag.tpe.typeSymbol.name.encodedName.toString
    val uncappedMethodName = String.valueOf(Character.toLowerCase(methodNameEnd.charAt(0))) + methodNameEnd.substring(1)
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Apply(Select(ev, TermName(uncappedMethodName)), List(lhs)), List(f.tree)))
  }

  def _f2[A, B, R](c: Context)(f: c.Expr[A])(g: c.Expr[B]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    val y = Apply(Select(ev, TermName(c.macroApplication.symbol.name.toString)), List(lhs))
    val toappl = Apply(y, List(f.tree)) 
    c.Expr[R](Apply(toappl, List(g.tree))) 
  }

  def unpack[T[_], A](c: Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(x)), List(ev)) => (ev, x)
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }

}
