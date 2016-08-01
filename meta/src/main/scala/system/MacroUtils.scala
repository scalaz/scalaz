package scalaz
package system

import scala.reflect.macros.whitebox.Context

object MacroUtils {
  def inline1(c: Context)(tree: c.Tree, x0: c.TermName): c.Tree = {
    import c.universe._

    val Function(List(ValDef(_, name, _, _)), body) = tree
    replaceTermName(c)(name, x0).transform(body)
  }

  def replaceTermName(c: Context)(from: c.TermName, to: c.TermName): c.universe.Transformer =  {
    import c.universe._

    new Transformer {
      override def transform(tree: Tree) = {
        super.transform(tree match {
          case Ident(name) if name == from  => Ident(to)
          case _                            => tree
        })
      }
    }
  }

  def unapply4(c: Context)(tpe: c.Type): Option[(c.Type, c.Type, c.Type, c.Type)] = {
    import c.universe._

    tpe match {
      case TypeRef(x, _, a :: b :: c :: d :: _)  => Some((a, b, c, d))
      case _                                     => None
    }
  }
}
