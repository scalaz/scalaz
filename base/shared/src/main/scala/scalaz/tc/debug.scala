package scalaz
package tc

import scala._

import scala.annotation.implicitAmbiguous
import scala.language.experimental.macros
import scala.language.implicitConversions

import Predef._
import data.Cord

/** A typeclass describing types which can be meaningfully represented as a `String`.
 */
trait DebugClass[A] {

  /** Produce a [[Cord]] representation of `a`.
   */
  def debug(a: A): Cord

  /** Produce a [[String]] representation of `a`.
   *
   * This should be equivalent to `debug(a).toString`.
   */
  def debugs(a: A): String
}

object DebugClass {

  // sorry
  trait DeriveDebug[A] extends DebugClass[A] with Alt[DeriveDebug[A]] {
    override def debug(a: A): Cord = Cord(debugs(a))
  }
  trait DeriveDebugs[A] extends DebugClass[A] with Alt[DeriveDebugs[A]] {
    override def debugs(a: A): String = Cord.toString(debug(a))
  }

  sealed trait Alt[A <: Alt[A]]

  /** A factory for `Debug` instances from functions to [[Cord]].
   */
  def instance[A](impl: A => Cord): Debug[A] =
    instanceOf(new DebugClass[A] with DebugClass.DeriveDebugs[A] {
      def debug(a: A) = impl(a)
    })

  implicit val booleanDebug: Debug[Boolean] = Debug.fromToString[Boolean]
  implicit val byteDebug: Debug[Byte]       = Debug.fromToString[Byte]
  implicit val doubleDebug: Debug[Double]   = Debug.fromToString[Double]
  implicit val floatDebug: Debug[Float]     = Debug.fromToString[Float]
  implicit val intDebug: Debug[Int]         = Debug.fromToString[Int]
  implicit val longDebug: Debug[Long]       = Debug.fromToString[Long]
  implicit val shortDebug: Debug[Short]     = Debug.fromToString[Short]
  implicit val stringDebug: Debug[String]   = Debug.fromToString[String]
  implicit val unitDebug: Debug[Unit]       = Debug.fromToString[Unit]

  implicit def listDebug[A: Debug]: Debug[List[A]] =
    DebugClass.instance(as => {
      def commaSep(tail: List[A], acc: Cord): Cord = tail match {
        case Nil     => acc
        case x :: xs => commaSep(xs, Cord.concat(acc, Cord.cons(",", Debug[A].debug(x))))
      }

      Cord.wrap("List(", as match {
        case Nil     => Cord.empty
        case x :: xs => commaSep(xs, Debug[A].debug(x))
      }, ")")
    })

  implicit def eitherDebug[L, R](implicit L: Debug[L], R: Debug[R]): Debug[Either[L, R]] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[Either[L, R]] {
      case Left(l)  => z"Left($l)"
      case Right(r) => z"Right($r)"
    }
  }

  implicit def optionDebug[A](implicit X: Debug[A]): Debug[Option[A]] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[Option[A]] {
      case Some(a) => z"Some($a)"
      case None    => Cord("None")
    }
  }

  /* https://github.com/scalaz/scalaz/pull/1633
  implicit def vectorDebug[A: Debug]: Debug[Vector[A]] = instanceOf(new DebugClass[Vector[A]] {
    override def show(as: Vector[A]) = {
      def commaSep(tail: Vector[A], acc: Cord): Cord = tail match {
        case Nil => acc
        case x :: xs => commaSep(xs, (acc :+ ",") ++ Debug[A].debug(x))
      }

      "[" +: (as match {
        case Nil => Cord()
        case x :: xs => commaSep(xs, Debug[A].debug(x))
      }) :+ "]"
    }
  })
   */

  implicit final def contravariantDebug: Contravariant[DebugClass] =
    instanceOf(new ContravariantClass[DebugClass] {
      def contramap[A, B](r: DebugClass[A])(f: B => A): DebugClass[B] =
        new DebugClass[B] {
          override def debugs(b: B) = r.debugs(f(b))
          override def debug(b: B)  = r.debug(f(b))
        }
    })
}

object Debug {
  def fromToString[A]: Debug[A]                = DebugClass.instance[A](a => Cord(a.toString))
  def apply[A](implicit A: Debug[A]): Debug[A] = A
}

trait DebugSyntax {
  implicit final class ToDebugOps[A](self: A) {
    def debug(implicit ev: Debug[A]): Cord = macro meta.Ops.i_0
    def debugs(implicit ev: Debug[A]): String = macro meta.Ops.i_0
  }

  implicit final def debugInterpolator(sc: StringContext): DebugInterpolator.Interpolator =
    new DebugInterpolator.Interpolator(sc)
}

object DebugInterpolator extends DebugInterpolator {
  implicit def mkHasDebug[A](x: A)(implicit D: Debug[A]): HasDebug =
    D.debug(x).asInstanceOf[HasDebug]

  final class Interpolator(val sc: StringContext) extends AnyVal {
    import scala.{ Seq => VarArgs }
    import StringContext.{ treatEscapes => escape }

    def z(args: HasDebug*): Cord =
      sc.parts
        .zipAll(args.asInstanceOf[VarArgs[Cord]], "", Cord.empty)
        .foldRight(Cord.empty) { (pa, res) =>
          val (part, arg) = pa
          Cord.cons(escape(part), Cord.concat(arg, res))
        }

    def zs(args: HasDebug*): String = Cord.toString(z(args: _*))
  }
}

private[tc] sealed class DebugInterpolator {
  type HasDebug // = Cord

  @implicitAmbiguous(
    "Cannot use the `z` interpolator to interpolate a value of type ${A}, as no implicit Debug[${A}] instance is in scope."
  )
  implicit def ambiguousDebug1[A](a: A): HasDebug =
    sys.error(s"Cannot use the `z` interpolator to interpolate ${a}, as no implicit Debug instance is in scope.")
  implicit def ambiguousDebug2[A](a: A): HasDebug =
    sys.error(s"Cannot use the `z` interpolator to interpolate ${a}, as no implicit Debug instance is in scope.")
}
