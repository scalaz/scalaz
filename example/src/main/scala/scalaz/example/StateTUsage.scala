package scalaz.example

import scalaz._

object FibStateExample {
  def main(args: Array[String]): Unit = {
    val S = scalaz.StateT.stateMonad[(Int, Int)]
    import S._ // for support if init, put, get, gets, ...
    import scalaz.syntax.monad._ // for support of replicateM

    val initialState = (0, 1)

    val (nextFib: State[(Int, Int), Int]) : State[(Int, Int), Int] = for {
      s <- get
      (a,b) = s
      n = a + b
      _ <- put ((b, n))
    } yield b // if we yield n, getNFibs gives you (1,2,3,5,8...)
              // yield b instead to get (1,1,2,3...)

    def getNFibs(k: Int): State[(Int, Int), IList[Int]] = {
      nextFib.replicateM(k)
    }

    def getNthFib(k:Int): State[(Int, Int), Int] = {
      if (k == 0)
        pure(0) // will be thrown away
      else
        getNthFib(k - 1) >> nextFib
    }

    // run two examples through the magic of App
    println( getNthFib(5).eval( initialState ) )
    println( getNFibs(10).eval( initialState ) )
  }
}

/** Simple call-by-need (i.e. lazy) interpreter for Lambda Calculus based off of
  * John Launchbury's "A Natural Semantics for Lazy Evaluation"
  * Uses the "Barendregt convention": All variable names are globally unique
  * (i.e. you cannot shadow variable names), and renames variables after substitution
  * to maintain this invariant.
  */
object LaunchburyInterpreter {
  import scalaz.std.list._
  import scalaz.std.string._
  import scalaz.syntax.monad._
  import scalaz.syntax.traverse._

  val S = scalaz.StateT.stateMonad[ReduceState]
  import S._

  /** Simple lambda calculus Abstract Syntax Tree.
    * Note that that apply applies a let-bound argument to an Expr.
    * This is to make sharing easier, by ensuring that arguments are in the heap.
    */
  abstract sealed class Expr
  case class Lambda(name: String, term: Expr) extends Expr
  case class Apply(term: Expr, arg:String) extends Expr
  case class Var(name: String) extends Expr
  case class Let(bindings: IMap[String, Expr], term: Expr) extends Expr

  case class ReduceState( heap: IMap[String, Expr]
                        , freshVars: LazyList[String]
                        )

  def main(args: Array[String]): Unit = {
    // \x.x
    val example1 = Lambda("x", Var("x"))
    // let z = \y.y in (\x.x) z
    val example2 = Let( IMap( "z" -> Lambda("y", Var("y")) )
                      , Apply(example1, "z")
                      )

    // run an example through the magic of App
    println(evaluate(example2))
  }

  private val initialState = ReduceState( IMap()
                                        , LazyList.from(1).map(x => "$" + x) // i.e. $1, $2, $3, ...
                                        )
  // Substitute new variable names in
  // e.g. sub(map("x" -> "y"), Var("x")) => Var("y")
  private def sub(m: IMap[String, String])(e: Expr): Expr = {
    val subExpr = sub(m) _
    def subName(n: String) : String = m lookup n getOrElse n
    e match {
      case Lambda(z, e2) => Lambda(subName(z), subExpr(e2))
      case Apply(e2, z)  => Apply(subExpr(e2), subName(z))
      case Var(z)        => Var(subName(z))
      case Let(bs, e2)   => Let( bs.foldlWithKey(IMap.empty[String, Expr])((m, k, v) => m + (subName(k) -> subExpr(v))), subExpr(e2))
    }
  }


  // replaces every bound variable with a new, "fresh" variable
  // e.g. freshen(Lambda("x", Var("x"))).eval(initialState) => Lambda("$1", Var("$1"))
  private def freshen(e: Expr): State[ReduceState, Expr] = {
    val getFreshVar : State[ReduceState, String] = for {
                            s <- get
                            ReduceState(_, f #:: fs) = s
                            _ <- modify(_.copy(freshVars = fs))
                          } yield f
    // Lambda and Let define new bound variables, so we substitute fresh variables into them
    // Var and Apply just recursively traverse the AST
    e match {
      case Lambda(x, e2) => for { y <- getFreshVar
                                  e3 <- freshen( sub(IMap(x -> y))(e2) )
                                } yield Lambda(y, e3)
      case Apply(e2, x)  => freshen(e2) >>= (e3 => pure(Apply(e3, x)))
      case Var(_)        => pure(e)
      case Let(bs, e2)   => for { fs <- getFreshVar.replicateM(bs.size)
                                  // IList[((originalVar, Expr), freshVar)]
                                  newBindings = bs.toIList.zip(fs)
                                  // sub(Map(originalVar -> freshVar))
                                  subs = sub( newBindings.map(tpl => tpl.copy(_1 = tpl._1._1)).toMap ) _
                                  // List[freshVar, Expr] - change to map when dolio's done
                                  bs2 = newBindings.map(tpl => tpl.copy(_2 = tpl._1._2, _1 = tpl._2)).toIList
                                  e3 <- freshen( subs(e2) )
                                  freshendBs <- bs2.traverseS{case (x,e) => freshen( subs(e) ).map((x,_))}.map(_.toMap)
                                } yield Let(freshendBs, e3)

    }
  }

  /** performs "big-step" reduction: a single call maps a term to its final result
    * reduces lambda-terms to whnf or "weak head normal form".  For our purposes,
    * whnf means a lambda term (generally, it also refers to primitives and constructors,
    * which we've omitted).
    */
  private def reduce(e:Expr): State[ReduceState, Expr] = {

    e match {
      case Lambda(x, e2) => pure(e) // as defined above, a Lambda is already in whnf
      case Apply(e2, x)  => reduce(e2) >>= { case Lambda(y, e3) => reduce( sub(IMap(y -> x))(e3) )
                                             case _ => sys.error("Ill-typed lambda term")
                                           }
      case Var(x)        => for { state <- get
                                  e2 = state.heap.lookup(x).getOrElse(sys.error(s"Invalid var reference $x"))
                                  _ <- modify(s => s.copy(heap = s.heap - x))
                                  e3 <- reduce(e2)
                                  _ <- modify(s => s.copy(heap = s.heap + ((x, e3))))
                                  freshendE <- freshen(e3)
                                } yield freshendE
      case Let(bs, e2)   => { val heapAdd = ((binding:(String, Expr)) =>
                                               modify(s => s.copy(heap = s.heap + binding)))
                              bs.toList.traverseS(heapAdd) >> reduce(e2)
                            }
    }
  }

  def evaluate(e:Expr): Expr = reduce(e).eval(initialState)
}
