package scalaz
package meta

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
 * parameter list(s) of the scalaz.data.typeclass method. Methods beginning with `fa_` have
 * their first parameter list prefixed by the target of the invocation, rather
 * than having the target in its own (first) parameter list. This is to support
 * scalaz.data.typeclass methods such as `Semigroup#append` and `Foldable#foldLeft`.
 */
class Ops(val c: blackbox.Context) {
  import c.universe._

  /* def method: R */
  def f_0: Tree = q"$ev.$name($lhs)"

  /* def method(a: A): R */
  def f_1(f: Tree): Tree = q"$ev.$name($lhs)($f)"

  /* def method(f: A): R */
  def fa_1(f: Tree): Tree = q"$ev.$name($lhs, $f)"

  /* def method(f: A)(g: B): R */
  def f_1_1(f: Tree)(g: Tree): Tree = q"$ev.$name($lhs)($f)($g)"

  /* def method(f: A)(g: B): R */
  def fa_1_1(f: Tree)(g: Tree): Tree = q"$ev.$name($lhs, $f)($g)"

  /* def method(f: A, g: B): R */
  def f_2(f: Tree, g: Tree): Tree = q"$ev.$name($lhs)($f, $g)"

  /** Destructured macro application.
   * - `ev`: the scalaz.data.typeclass evidence
   * - `name`: the name of the invoked method
   * - `lhs`: the invocation target of the call
   */
  private lazy val (ev, name, lhs) =
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(x)), List(ev)) =>
        (ev, c.macroApplication.symbol.name.toTermName, x)
      case t => c.abort(c.enclosingPosition, s"Cannot extract subject of operation (tree = $t)")
    }
}

/* Version of the zero-cost syntax macro that allows you to pass
 * the method name in as a type.
 */
class SymOps(val c: blackbox.Context) {
  import c.universe._

  /* def method: R */
  def f_0[T](implicit T: c.WeakTypeTag[T]): Tree = {
    val (ev, name, lhs) = unpack[T]
    q"$ev.$name($lhs)"
  }

  /* def method(a: A): R */
  def f_1[T](f: Tree)(implicit T: c.WeakTypeTag[T]): Tree = {
    val (ev, name, lhs) = unpack[T]
    q"$ev.$name($lhs)($f)"
  }

  /* def method(f: A): R */
  def fa_1[T](f: Tree)(implicit T: c.WeakTypeTag[T]): Tree = {
    val (ev, name, lhs) = unpack[T]
    q"$ev.$name($lhs, $f)"
  }

  /* def method(f: A)(g: B): R */
  def f_1_1[T](f: Tree)(g: Tree)(implicit T: c.WeakTypeTag[T]): Tree = {
    val (ev, name, lhs) = unpack[T]
    q"$ev.$name($lhs)($f)($g)"
  }

  /* def method(f: A)(g: B): R */
  def fa_1_1[T](f: Tree)(g: Tree)(implicit T: c.WeakTypeTag[T]): Tree = {
    val (ev, name, lhs) = unpack[T]
    q"$ev.$name($lhs, $f)($g)"
  }

  /* def method(f: A, g: B): R */
  def f_2[T](f: Tree, g: Tree)(implicit T: c.WeakTypeTag[T]): Tree = {
    val (ev, name, lhs) = unpack[T]
    q"$ev.$name($lhs)($f, $g)"
  }

  /** Destructured macro application.
   * - `ev`: the scalaz.data.typeclass evidence
   * - `name`: the name of the invoked method
   * - `lhs`: the invocation target of the call
   */
  private def unpack[T](implicit T: c.WeakTypeTag[T]): (c.Tree, TermName, c.Tree) = {
    val (ev, lhs) = c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(lhs)), List(ev)) =>
        (ev, lhs)
      case t => c.abort(c.enclosingPosition, s"Cannot extract subject of operation (tree = $t)")
    }

    val methodName = T.tpe.typeSymbol.name.encodedName.toString
    (ev, TermName(methodName), lhs)
  }
}

/** Versions of the zero-cost macros for when the receiver is not an instance
 * of the scalaz.data.typeclass in question.
 */
class IdOps(val c: blackbox.Context) {
  import c.universe._

  /* def method(x: A): R */
  def id_1(x: Tree): Tree = q"$x.$name($lhs)"

  private lazy val lhs =
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) => lhs
      case t                                 => c.abort(c.enclosingPosition, s"Cannot extract subject of operation (tree = $t)")
    }

  private lazy val name =
    c.macroApplication.symbol.name.toTermName
}
