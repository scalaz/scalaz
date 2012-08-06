package scalaz.example
package transformers
package typecheck

object TypeCheckerWithExplicitTypesAST {

  trait Literal
  case class Num(i: Int) extends Literal
  case class Bool(b: Boolean) extends Literal

  sealed trait Exp
  case class Id(name: String) extends Exp
  case class Fun(arg: String, argType: Type, body: Exp) extends Exp
  case class App(f: Exp, arg: Exp) extends Exp
  case class Lit(l: Literal) extends Exp
  case class If(tst: Exp, thn: Exp, els: Exp) extends Exp

  trait Type
  case class TyLam(f: Type, arg: Type) extends Type
  case class TyBuiltin(name: String) extends Type
  case class TyVar(name: String) extends Type

  type TypeEnv = Map[String, Type]

  val numT  = TyBuiltin("Num")
  val boolT = TyBuiltin("Bool")

  def litToTy(l: Literal): Type = l match {
    case Num(_)  => numT
    case Bool(_) => boolT
  }

  val predef: TypeEnv = Map(
    "+"  -> (TyLam(numT, TyLam(numT, numT))),
    "-"  -> (TyLam(numT, TyLam(numT, numT))),
    "==" -> (TyLam(numT, TyLam(numT, boolT))),
    "&&" -> (TyLam(boolT, TyLam(boolT, boolT))),
    "||" -> (TyLam(boolT, TyLam(boolT, boolT)))
  )
}
