import sbt._

object TupleNInstances {

  def apply(outputDir: File): File = {
    val header = "package scalaz\npackage std\n\n"
    val source = header + (2 to 8).map{ n =>
      tupleNTraverse(n) + tupleNBindRec(n) + tupleNMonad(n)
    }.mkString("\n")

    val file = outputDir / "scalaz" / "std" / "TupleNInstances.scala"
    IO.write(file, source)
    file
  }

  def tupleNTraverse(n: Int): String = {
    val tparams = (1 until n).map("A" + _).mkString(", ")
    val fa = (1 until n).map("fa._" + _).mkString(", ")

    s"""
private[std] trait Tuple${n}Functor[$tparams] extends Traverse[($tparams, ?)] {
  override final def map[A, B](fa: ($tparams, A))(f: A => B) =
    ($fa, f(fa._$n))
  override final def traverseImpl[G[_], A, B](fa: ($tparams, A))(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(f(fa._$n))(($fa, _))
}
"""
  }

  def tupleNBindRec(n: Int): String = {
    val tparams = (1 until n).map("A" + _).mkString(", ")
    val xs = (1 until n).map("x" + _).mkString(", ")
    val zs = (1 until n).map("z" + _).mkString(", ")

s"""
private[std] trait Tuple${n}BindRec[$tparams] extends BindRec[($tparams, ?)] with Tuple${n}Functor[$tparams] {
  ${(1 until n).map(i => s"def _$i : Semigroup[A$i]").mkString("; ")}

  override def bind[A, B](fa: ($tparams, A))(f: A => ($tparams, B)) = {
    val t = f(fa._$n)
    (${(1 until n).map(i => s"_$i.append(fa._$i, t._$i)").mkString(", ")}, t._$n)
  }

  override def tailrecM[A, B](f: A => ($tparams, A \\/ B))(a: A): ($tparams, B) = {
    @annotation.tailrec
    def go(${(1 until n).map(i => s"s$i: A$i").mkString(", ")})(z: A): ($tparams, B) =
      f(z) match {
        case (${(1 until n).map("a" + _).mkString(", ")}, b0) =>
          ${(1 until n).map(i => s"val x$i = _$i.append(s$i, a$i)").mkString("; ")}
          b0 match {
            case -\\/(a0) => go($xs)(a0)
            case \\/-(b1) => ($xs, b1)
          }
      }
    val ($zs, e) = f(a)
    e match {
      case -\\/(a0) => go($zs)(a0)
      case \\/-(b) => ($zs, b)
    }
  }
}
"""
  }

  def tupleNMonad(n: Int): String = {
    val tparams = (1 until n).map("A" + _).mkString(", ")

s"""
private[std] abstract class Tuple${n}Monad[$tparams] extends Monad[($tparams, ?)] with Tuple${n}BindRec[$tparams] {
  ${(1 until n).map(i => s"override def _$i : Monoid[A$i]").mkString("; ")}
  def point[A](a: => A) = (${(1 until n).map(i => s"_$i.zero").mkString(", ")}, a)
}
"""
  }
}
