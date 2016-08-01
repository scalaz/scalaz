package scalaz
package data

import scala.reflect.macros.whitebox.Context

import system.MacroUtils.inline1

object LensMacros {
  def slens[S: c.WeakTypeTag, A: c.WeakTypeTag](c: Context)
      (sa: c.Expr[S => A])(sas: c.Expr[S => A => S]): c.Expr[Lens[S, S, A, A]] =
    lens[S, S, A, A](c)(sa)(sas)

  def lens[S: c.WeakTypeTag, T: c.WeakTypeTag, A: c.WeakTypeTag, B: c.WeakTypeTag](c: Context)
      (sa: c.Expr[S => A])(sbt: c.Expr[S => B => T]): c.Expr[Lens[S, T, A, B]] = {
    import c.universe._
    import Flag._

    val (s, t, a, b) = (c.weakTypeOf[S], c.weakTypeOf[T], c.weakTypeOf[A], c.weakTypeOf[B])

    val sName = TermName(c.freshName("s"))
    val bName = TermName(c.freshName("b"))
    val sa1   = inline1(c)(c.untypecheck(sa.tree.duplicate), sName)
    val sbt1  = inline1(c)(inline1(c)(c.untypecheck(sbt.tree.duplicate), sName), bName)

    c.Expr[Lens[S, T, A, B]](q"""
      new Lens[$s, $t, $a, $b] {
          def get($sName: $s): $a = $sa1
          def set($sName: $s, $bName: $b): $t = $sbt1
        }
    """)
  }
}
