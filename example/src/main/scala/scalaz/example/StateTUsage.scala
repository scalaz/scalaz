package scalaz.example

import scalaz._

object StateTUsage extends App {
  import StateT._

  def f[M[+_]: Functor] {
    Functor[({type l[a] = StateT[M, Int, a]})#l]
  }

  def p[M[+_]: Pointed] {
    Functor[({type l[a] = StateT[M, Int, a]})#l]
    Pointed[({type l[a] = StateT[M, Int, a]})#l]
  }

  def m[M[+_]: Monad] {
    Applicative[({type l[a] = StateT[M, Int, a]})#l]
    Monad[({type l[a] = StateT[M, Int, a]})#l]
    MonadState[({type f[s, a] = StateT[M, s, a]})#f, Int]
  }

  def state() {
    val state: State[String, Int] = State((x: String) => (x + 1, 0))
    val eval: Int = state.eval("")
    state.flatMap(_ => state)
  }
}

object FibStateExample extends App {
  val S = scalaz.StateT.stateMonad[(Int, Int)]
  import S.monadSyntax._
  import scalaz.State._
  import scalaz.std.list._

  val initialState = (0, 1)

  val (nextFib: State[(Int, Int), Int]) = for {
    s <- init:State[(Int, Int), (Int, Int)]
    val (a,b) = s
    val n = a + b
    _ <- put (b, n)
  } yield b // if we yield n, getNFibs gives you (1,2,3,5,8...)
            // yield b instead to get (1,1,2,3...)

  def getNFibs(k: Int): State[(Int, Int), List[Int]] = {
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

/** Simple call-by-need (i.e. lazy) interpreter for Lambda Calculus based off of
  * John Launchbury's "A Natural Semantics for Lazy Evaluation"
  * Uses the "Barendregt convention": All variable names are globally unique
  * (i.e. you cannot shadow variable names), and renames variables after substitution
  * to maintain this invariant.
  */
object LaunchburyInterpreter extends App {
  import scala.collection.immutable.HashMap
  import scalaz.StreamT._
  import scalaz.std.function._
  import scalaz.std.list._
  import scalaz.std.map._
  import scalaz.syntax.foldable._
  import scalaz.syntax.traverse._
  import scalaz.syntax.arrow._

  val S = scalaz.StateT.stateMonad[ReduceState]
  import S.monadSyntax._
  import scalaz.State._

  /** Simple lambda calculus Abstract Syntax Tree.
    * Note that that apply applies a let-bound argument to an Expr.
    * This is to make sharing easier, by ensuring that arguments are in the heap.
    */
  abstract sealed class Expr
  case class Lambda(name: String, term: Expr) extends Expr
  case class Apply(term: Expr, arg:String) extends Expr
  case class Var(name: String) extends Expr
  case class Let(bindings: Map[String, Expr], term: Expr) extends Expr

  // \x.x
  val example1 = Lambda("x", Var("x"))
  // let z = \y.y in (\x.x) z
  val example2 = Let( HashMap( "z" -> Lambda("y", Var("y")) )
                    , Apply(example1, "z")
                    )

  case class ReduceState( heap: Map[String, Expr]
                        , freshVars: Stream[String]
                        )

  private val initialState = ReduceState( HashMap()
                                        , Stream.from(1).map(x => "$" + x) // i.e. $1, $2, $3, ...
                                        )
  // Substitute new variable names in
  // e.g. sub(map("x" -> "y"), Var("x")) => Var("y")
  private def sub(m: Map[String, String])(e: Expr): Expr = {
    val subExpr = sub(m) _
    def subName(n: String) = if (m contains n) m(n) else n
    e match {
      case Lambda(z, e2) => Lambda(subName(z), subExpr(e2))
      case Apply(e2, z)  => Apply(subExpr(e2), subName(z))
      case Var(z)        => Var(subName(z))
      case Let(bs, e2)   => Let( bs.map(subName _ *** subExpr), subExpr(e2))
    }
  }


  // replaces every bound variable with a new, "fresh" variable
  // e.g. freshen(Lambda("x", Var("x"))).eval(initialState) => Lambda("$1", Var("$1"))
  private def freshen(e: Expr): State[ReduceState, Expr] = {
    val getFreshVar = for { s <- init: State[ReduceState,ReduceState]
                            val ReduceState(_, f #:: fs) = s
                            _ <- modify((s:ReduceState) => s.copy(freshVars = fs))
                          } yield f
    // Lambda and Let define new bound variables, so we substitute fresh variables into them
    // Var and Apply just recursively traverse the AST
    e match {
      case Lambda(x, e2) => for { y <- getFreshVar
                                  e3 <- freshen( sub(HashMap(x -> y))(e2) )
                                } yield Lambda(y, e3)
      case Apply(e2, x)  => freshen(e2) >>= (e3 => pure(Apply(e3, x)))
      case Var(_)        => pure(e)
      case Let(bs, e2)   => for { fs <- getFreshVar.replicateM(bs.size)
                                  // Seq[((originalVar, Expr), freshVar)]
                                  val newBindings = bs.toSeq.zip(fs)
                                  // sub(Map(originalVar -> freshVar))
                                  val subs = sub( newBindings.map(tpl => tpl.copy(_1 = tpl._1._1)).toMap ) _
                                  // List[freshVar, Expr] - change to map when dolio's done
                                  val bs2 = newBindings.map(tpl => tpl.copy(_2 = tpl._1._2, _1 = tpl._2)).toList
                                  e3 <- freshen( subs(e2) )
                                  freshendBs <- bs2.traverseS{case (x,e) => freshen( subs(e) ).map((x,_))}.map(_.toMap)
                                } yield Let(freshendBs, e3)

    }
  }

  /** performs "big-step" reduction: a single call maps a term to its final result
    * reduces lambda-terms to whnf or "weak head normal form".  For our purposes,
    * whnf means a lambda term (generally, it also refers to primitives and constructors,
    * which we've ommitted).
    */
  private def reduce(e:Expr): State[ReduceState, Expr] = {

    e match {
      case Lambda(x, e2) => pure(e) // as defined above, a Lambda is already in whnf
      case Apply(e2, x)  => reduce(e2) >>= { case Lambda(y, e3) => reduce( sub(HashMap(y -> x))(e3) )
                                             case _ => sys.error("Ill-typed lambda term")
                                           }
      case Var(x)        => for { state <- init:State[ReduceState, ReduceState]
                                  val e2 = state.heap(x)
                                  _ <- modify((s:ReduceState) => s.copy(heap = s.heap - x))
                                  e3 <- reduce(e2)
                                  _ <- modify((s:ReduceState) => s.copy(heap = s.heap + ((x, e3))))
                                  freshendE <- freshen(e3)
                                } yield freshendE
     case Let(bs, e2)   => { val heapAdd = ((binding:(String, Expr)) =>
                                              modify((s:ReduceState) => s.copy(heap = s.heap + binding)))
                             bs.toList.traverseS(heapAdd) >> reduce(e2)
                           }
   }
 }

 def evaluate(e:Expr): Expr = reduce(e).eval(initialState)
 // run an example through the magic of App
 println(evaluate(example2))
}

