package scalaz.example
package transformers
package typecheck

object TypeCheckerWithExplicitTypes {

  import TypeCheckerWithExplicitTypesAST._

  def success(t: Type) = t
  def typeError(msg: String) = sys.error(msg)

  def find(s: String, env: TypeEnv): Type =
    env.find(_._1 == s).map(p => success(p._2)).getOrElse(sys.error("not found: " + s))

  def compare(t1: Type, t2: Type, resultType: Type, errorMsg: String): Type =
    if (t1 == t2) success(resultType) else typeError(errorMsg)

  // the real type check function, which works with the type environment.
  def typeCheck(expr: Exp, env: TypeEnv = predef): Type = expr match {
    case Lit(v) => success(litToTy(v))
    case Id(x) => find(x, env)
    // make sure the first branch is a boolean and then
    // make sure the second and third branches have the same type
    case If(tst, texp, fexp) =>
      val t   = typeCheck(tst, env)
      val _   = compare(t, boolT, boolT, "error: if required bool in test position, but got: " + t)
      val lt  = typeCheck(texp, env)
      val rt  = typeCheck(fexp, env)
      val res = compare(lt, rt, lt, "error: if branches not the same type, got: " + (lt, rt))
      res
    case Fun(arg, argType, body) =>
      val t = typeCheck(body, env + (arg -> argType))
      TyLam(argType, t)
    // make sure the first argument to function application is indeed a function
    // then make sure that the arguments match the explicit declarations
    case App(operator, operand) =>
      val operatorType = typeCheck(operator, env)
      val operandType  = typeCheck(operand, env)
      val res          = operatorType match {
        case TyLam(argType, resultType) =>
          compare(argType, operandType, resultType,
            "function expected arg of type: " + argType + ", but got: " + operandType)
        case t => typeError("function application expected function, but got: " + t)
      }
      res
  }
}
