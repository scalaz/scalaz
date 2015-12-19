package scalaz.example

/**Examples showing the use of Unapply to reduce the need for type annotations */
object UnapplyInference extends App {
  eitherTBifunctor()

  def eitherTBifunctor() {
    import scalaz._, Scalaz._

    val either: (Int \/ Int) = \/.right(1)
    val eitherT = EitherT(some(either))

    println((eitherT :-> (_ - 1)).run) // Some(Right(0))
  }

  def eitherTBitraverse() {
    import scalaz._
    import std.list._, std.option._
    import syntax.all._

    val either: (List[Int] \/ List[Int]) = \/.right(List(1))
    val eitherT: EitherT[Option, List[Int], List[Int]] = EitherT(some(either))

    val bisequence: List[EitherT[Option, Int, Int]] = eitherT.bisequence[List, Int, Int]
  }

  // Without Unapply
  def stateTraverse1 {
    import scalaz._, Scalaz._
    val ls = List(1, 2, 3)
    val traverseOpt: Option[List[Int]] = ls.traverse(a => some(a))
    val traverseState: State[Int, List[Int]] = ls.traverse[State[Int,?],Int](a => State((x: Int) => (x+1,a)))
  }

  // With Unapply (in the signature of traverseU)
  def stateTraverse2 {
    import scalaz._, Scalaz._

    val ls = List(1, 2, 3)
    val traverseOpt: Option[List[Int]] = ls.traverseU(a => some(a))
    val traverseState = ls.traverseU(a => State((x: Int) => (x + 1, a)))

    val pair: State[Int, (Int, Int)] = State((x: Int) => (x + 1, x)).tuple(State((x: Int) => (x + 2, x)))
  }

  def kleisliCompose() {
    import scalaz._
    import std.option._
    import syntax.compose._
    import Kleisli._

    val k = kleisli((a: Int) => some(a + 1))
    k >>> k
  }

  def kleisliU() {
    import scalaz._
    val k: Kleisli[NumberFormatException \/ ?, String, Int] =
      Kleisli.kleisliU{s: String => try \/-(s.toInt) catch{ case e: NumberFormatException => -\/(e) }}
  }

  def functorSyntaxChaining() {
    import scalaz._
    import syntax.functor._

    val e: String \/ Int = \/-(1)

    ToFunctorOps[String \/ ?, Int](e.map(1 +)).map(1 +)
    ToFunctorOpsUnapply(e.map(1 +)).map(1 +)

    e.map(1 +).map(1 +)

    import std.tuple._

    (1, 2).map(1+).map(1+)
    (1, 2, 3).map(1+).map(1+)
    (1, 2, 3, 4).map(1+).map(1+)
    (1, 2, 3, 4, 5).map(1+).map(1+)
    (1, 2, 3, 4, 5, 6).map(1+).map(1+)
    (1, 2, 3, 4, 5, 6, 7).map(1+).map(1+)
  }
}
