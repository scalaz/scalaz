package scalaz.example
package transformers
package typecheck

object TypeCheckerWithExplicitTypes_Monadic {

  import TypeCheckerWithExplicitTypesAST._
  import scalaz.{\/, \/-, -\/}

  def success(t: Type): String \/ Type = \/-(t)
  def typeError(msg: String): String \/ Type = -\/(msg)

  def find(s: String, env: TypeEnv): String \/ Type =
    env.find(_._1 == s).map(p => success(p._2)).getOrElse(typeError("not found: " + s))

  def compare(t1: Type, t2: Type, resultType: Type, errorMsg: String): String \/ Type =
    if (t1 == t2) success(resultType) else typeError(errorMsg)

  // the real type check function, which works with the type environment.
  def typeCheck(expr: Exp, env: TypeEnv = predef): String \/ Type = expr match {
    case Lit(v) => success(litToTy(v))
    case Id(x) => find(x, env)
    // make sure the first branch is a boolean and then
    // make sure the second and third branches have the same type
    case If(tst, texp, fexp) => for {
      t   <- typeCheck(tst, env)
      _   <- compare(t, boolT, boolT, "if required bool in test position, but got: " + t)
      lt  <- typeCheck(texp, env)
      rt  <- typeCheck(fexp, env)
      res <- compare(lt, rt, lt, "if branches not the same type, got: " + (lt, rt))
    } yield res
    case Fun(arg, argType, body) => for {
      t <- typeCheck(body, env + (arg -> argType))
    } yield TyLam(argType, t)
    // make sure the first argument to function application is indeed a function
    // then make sure that the arguments match the explicit declarations
    case App(operator, operand) => for {
      operatorType <- typeCheck(operator, env)
      operandType  <- typeCheck(operand, env)
      res          <- operatorType match {
        case TyLam(argType, resultType) =>
          compare(argType, operandType, resultType,
            "function expected arg of type: " + argType + ", but got: " + operandType)
        case _ => typeError("function application expected function, but got: " + operatorType)
      }
    } yield res
  }
}
