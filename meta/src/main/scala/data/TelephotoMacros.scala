package scalaz
package data

import scala.reflect.macros.whitebox.Context

import system.MacroUtils.unapply4

object TelephotoMacros {
  def compose2[
    O0: c.WeakTypeTag, O1: c.WeakTypeTag,
    O: c.WeakTypeTag, OL: c.WeakTypeTag
  ](c: Context)(
    o0: c.Expr[O0], o1: c.Expr[O1]
  )(OL: c.Expr[OL]): c.Expr[O] = {
    import c.universe._
    import Flag._

    val os = List(o1.tree, o0.tree).toVector
    val xs = List(c.weakTypeOf[O1], c.weakTypeOf[O0])

    _compose[O](c)(xs)(os(_))
  }

  // TODO Add 2 to 9 arities

  def composeA[
    O0: c.WeakTypeTag, O1: c.WeakTypeTag, O2: c.WeakTypeTag, O3: c.WeakTypeTag, O4: c.WeakTypeTag,
    O5: c.WeakTypeTag, O6: c.WeakTypeTag, O7: c.WeakTypeTag, O8: c.WeakTypeTag, O9: c.WeakTypeTag,
    O: c.WeakTypeTag, OL: c.WeakTypeTag
  ](c: Context)(
    o0: c.Expr[O0], o1: c.Expr[O1], o2: c.Expr[O2], o3: c.Expr[O3], o4: c.Expr[O4],
    o5: c.Expr[O5], o6: c.Expr[O6], o7: c.Expr[O7], o8: c.Expr[O8], o9: c.Expr[O9]
  )(OL: c.Expr[OL]): c.Expr[O] = {
    import c.universe._
    import Flag._

    val os = List(o9.tree, o8.tree, o7.tree, o6.tree, o5.tree, o4.tree, o3.tree, o2.tree, o1.tree, o0.tree).toVector
    val xs = List(c.weakTypeOf[O9], c.weakTypeOf[O8], c.weakTypeOf[O7], c.weakTypeOf[O6], c.weakTypeOf[O5],
                  c.weakTypeOf[O4], c.weakTypeOf[O3], c.weakTypeOf[O2], c.weakTypeOf[O1], c.weakTypeOf[O0])

    _compose[O](c)(xs)(os(_))
  }

  def _compose[O: c.WeakTypeTag](c: Context)(xs: List[c.Type])(get: Int => c.Tree): c.Expr[O] = {
    import c.universe._
    import Flag._

    val o = c.weakTypeOf[O]
    val Some((s, t, a, b)) = unapply4(c)(o)
    val is = (0 to (xs.size -1))

    def lift(init: TermName => Tree): TermName => Tree =
      is.tail.foldLeft(init) { (f, i) => s => q"${get(i)}.modify($s)(a => ${f(TermName("a"))})" }

    val getter = is.reverse.foldLeft(Ident(TermName("s")): Tree) { (tree, i) => q"${get(i)}.get($tree)" }
    val setter = lift(s => q"${get(0)}.set($s, b)")(TermName("s"))
    val modifier = lift(s => q"${get(0)}.modify($s)(f)")(TermName("s"))

    c.Expr[O](q"""
      new Lens[$s, $t, $a, $b] {
          def get(s: $s): $a = $getter
          def set(s: $s, b: $b): $t = $setter
          override def modify(s: $s)(f: $a => $b): $t = $modifier
        }
    """)
  }

  def composeHList[OL <: HList: c.WeakTypeTag, O: c.WeakTypeTag](c: Context): c.Expr[O] = {
    import c.universe._
    import Flag._

    def hunfold(tpe: Type): List[Type] = tpe match {
      case TypeRef(x, _, a :: b :: _) => a :: hunfold(b)
      case x => Nil
    }

    def hselect(tree: Tree, i: Int): Tree = i match {
      case 0 => Select(tree, TermName("head"))
      case i => hselect(Select(tree, TermName("tail")), i - 1)
    }

    val xs = hunfold(c.weakTypeOf[OL])
    val Apply(Apply(TypeApply(_, _), List(opticsVal)), _) = c.prefix.tree

    _compose[O](c)(xs)(hselect(opticsVal, _))
  }
}
