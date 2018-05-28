package scalaz
package meta

import scala.{ List, StringContext }
import scala.reflect.macros.blackbox

// Originally inspired by http://typelevel.org/blog/2013/10/13/spires-ops-macros.html

/** Zero-cost syntax ops macros.
 *
 * Inside of a syntax class, these macros can be used as the definition of
 * extension methods to entirely eliminate the allocation of the syntax class
 * and the implicit conversion.
 *
 * After defining
 * {{{
 * class Ops[A: Typeclass](a: A) {
 *   def method[B](b: B): Result = macro Ops.f_1
 * }
 * }}}
 *
 * then calling `a.method(b)` (if `a`'s type has a `Typeclass` instance) will
 * expand to `tcA.method(a)(b)` (where `tcA : Typeclass[A]` is the implicit
 * evidence).
 *
 * Due to the way that scala.reflect macros work, there is no shape-polymorphic
 * macro implementation. The methods are named according to the shape of the
 * parameter list(s) of the typeclass method. Methods beginning with `fa_` have
 * their first parameter list prefixed by the target of the invocation, rather
 * than having the target in its own (first) parameter list. This is to support
 * typeclass methods such as `Semigroup#append` and `Foldable#foldLeft`.
 */
class Ops(val c: blackbox.Context) {
  import c.universe._

  /* def method: R */
  def f_0: Tree                              = q"$ev.$methodName   ($lhs)"
  def i_0(ev: Tree): Tree                    = q"$ev.$methodName   ($lhs)"
  def n_0[T: c.WeakTypeTag]: Tree            = q"$ev.${typeName[T]}($lhs)"
  def ni_0[T: c.WeakTypeTag](ev: Tree): Tree = q"$ev.${typeName[T]}($lhs)"

  /* def method(a: A): R */
  def f_1(f: Tree): Tree                              = q"$ev.$methodName   ($lhs)($f)"
  def i_1(f: Tree)(ev: Tree): Tree                    = q"$ev.$methodName   ($lhs)($f)"
  def n_1[T: c.WeakTypeTag](f: Tree): Tree            = q"$ev.${typeName[T]}($lhs)($f)"
  def ni_1[T: c.WeakTypeTag](f: Tree)(ev: Tree): Tree = q"$ev.${typeName[T]}($lhs)($f)"

  /* def method(f: A): R */
  def fa_1(f: Tree): Tree                              = q"$ev.$methodName   ($lhs, $f)"
  def ia_1(f: Tree)(ev: Tree): Tree                    = q"$ev.$methodName   ($lhs, $f)"
  def na_1[T: c.WeakTypeTag](f: Tree): Tree            = q"$ev.${typeName[T]}($lhs, $f)"
  def nia_1[T: c.WeakTypeTag](f: Tree)(ev: Tree): Tree = q"$ev.${typeName[T]}($lhs, $f)"

  /* def method(f: A)(g: B): R */
  def f_1_1(f: Tree)(g: Tree): Tree                              = q"$ev.$methodName   ($lhs)($f)($g)"
  def i_1_1(f: Tree)(g: Tree)(ev: Tree): Tree                    = q"$ev.$methodName   ($lhs)($f)($g)"
  def n_1_1[T: c.WeakTypeTag](f: Tree)(g: Tree): Tree            = q"$ev.${typeName[T]}($lhs)($f)($g)"
  def ni_1_1[T: c.WeakTypeTag](f: Tree)(g: Tree)(ev: Tree): Tree = q"$ev.${typeName[T]}($lhs)($f)($g)"

  /* def method(f: A)(g: B): R */
  def fa_1_1(f: Tree)(g: Tree): Tree                              = q"$ev.$methodName   ($lhs, $f)($g)"
  def ia_1_1(f: Tree)(g: Tree)(ev: Tree): Tree                    = q"$ev.$methodName   ($lhs, $f)($g)"
  def na_1_1[T: c.WeakTypeTag](f: Tree)(g: Tree): Tree            = q"$ev.${typeName[T]}($lhs, $f)($g)"
  def nia_1_1[T: c.WeakTypeTag](f: Tree)(g: Tree)(ev: Tree): Tree = q"$ev.${typeName[T]}($lhs, $f)($g)"

  /* def method(f: A)(implicit g: B): R */
  def i_1_1i(f: Tree)(g: Tree, ev: Tree): Tree                    = q"$ev.$methodName   ($lhs)($f)($g)"
  def ni_1_1i[T: c.WeakTypeTag](f: Tree)(g: Tree, ev: Tree): Tree = q"$ev.${typeName[T]}($lhs)($f)($g)"

  /* def method(f: A, g: B): R */
  def f_2(f: Tree, g: Tree): Tree                              = q"$ev.$methodName   ($lhs)($f, $g)"
  def i_2(f: Tree, g: Tree)(ev: Tree): Tree                    = q"$ev.$methodName   ($lhs)($f, $g)"
  def n_2[T: c.WeakTypeTag](f: Tree, g: Tree): Tree            = q"$ev.${typeName[T]}($lhs)($f, $g)"
  def ni_2[T: c.WeakTypeTag](f: Tree, g: Tree)(ev: Tree): Tree = q"$ev.${typeName[T]}($lhs)($f, $g)"

  /// Name of the type.
  private def typeName[T](implicit T: c.WeakTypeTag[T]) =
    TermName(T.tpe.typeSymbol.name.encodedName.toString)

  // Name of the invoked method.
  private def methodName: TermName =
    c.macroApplication.symbol.name.toTermName

  // Typeclass evidence.
  private def ev: Tree = c.prefix.tree match {
    case Apply(Apply(TypeApply(_, _), List(_)), List(ev)) => ev
    case t =>
      c.abort(c.enclosingPosition, s"Cannot extract subject of operation (tree = $t)")
  }

  // Invocation target of the call.
  private def lhs: Tree = c.prefix.tree match {
    case Apply(Apply(TypeApply(_, _), List(lhs)), List(_)) => lhs
    case Apply(TypeApply(_, _), List(lhs))                 => lhs
    case t =>
      c.abort(c.enclosingPosition, s"Cannot extract subject of operation (tree = $t)")
  }
}
