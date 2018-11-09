package scalaz

import org.scalacheck.Prop.forAll
import Tags._

object MaybeTest extends SpecLite {
  import scalaz.scalacheck.ScalazProperties._
  import scalaz.scalacheck.ScalazArbitrary._
  import std.anyVal._
  import std.string._
  import syntax.equal._

  import Maybe._

  checkAll("Maybe", order.laws[Maybe[Int]])
  checkAll("Maybe @@ First", order.laws[FirstMaybe[Int]])
  checkAll("Maybe @@ Last", order.laws[LastMaybe[Int]])
  checkAll("Maybe @@ Min", order.laws[MinMaybe[Int]])
  checkAll("Maybe @@ Max", order.laws[MaxMaybe[Int]])

  checkAll("Maybe", monoid.laws[Maybe[Int]])
  checkAll("Maybe @@ First", monoid.laws[FirstMaybe[Int]])
  checkAll("Maybe @@ Last", monoid.laws[LastMaybe[Int]])
  checkAll("Maybe @@ Min", monoid.laws[MinMaybe[Int]])
  checkAll("Maybe @@ Max", monoid.laws[MaxMaybe[Int]])

  checkAll("Band[Maybe @@ First]", band.laws[FirstMaybe[Int]])
  checkAll("Band[Maybe @@ Last]", band.laws[LastMaybe[Int]])
  checkAll("Band[Maybe @@ Min]", band.laws[MinMaybe[Int]])
  checkAll("Band[Maybe @@ Max]", band.laws[MaxMaybe[Int]])

  checkAll("Maybe @@ First", monad.laws[FirstMaybe])
  checkAll("Maybe @@ Last", monad.laws[LastMaybe])
  checkAll("Maybe @@ Min", monad.laws[MinMaybe])
  checkAll("Maybe @@ Max", monad.laws[MaxMaybe])

  checkAll(bindRec.laws[Maybe])
  checkAll(monadPlus.strongLaws[Maybe])
  checkAll(traverse.laws[Maybe])
  checkAll(zip.laws[Maybe])
  checkAll(isEmpty.laws[Maybe])
  checkAll(cobind.laws[Maybe])
  checkAll(align.laws[Maybe])
  checkAll(equal.laws[Maybe[Int]])
  checkAll(semilattice.laws[Maybe[ISet[Int]]])

  "Empty is less than anything else" ! forAll { x: Maybe[Int] => Order[Maybe[Int]].greaterThanOrEqual(x, Maybe.empty) }

  "Empty is ignored in Maybe[A]@@Min" ! forAll { x: Maybe[Int] =>
    import syntax.monoid._
    (Min(x) |+| Min(empty)) must_=== Min(x)
  }

  "Empty is ignored in Maybe[A]@@Max" ! forAll { x: Maybe[Int] =>
    import syntax.monoid._
    (Max(x) |+| Max(empty)) must_=== Max(x)
  }

  "Preserved through Option" ! forAll { x: Maybe[Int] => std.option.toMaybe(x.toOption) === x }

  "just toFailure is failure" ! forAll { (x: Int, s: String) => just(x).toFailure(s).isFailure }

  "empty toFailure is success" ! forAll { s: String => empty.toFailure(s).isSuccess }

  "just toSuccess is success" ! forAll { (x: Int, s: String) => just(x).toSuccess(s).isSuccess }

  "empty toSuccess is failure" ! forAll { s: String => empty.toSuccess(s).isFailure }

  "just toLeft is left" ! forAll { (x: Int, s: String) => just(x).toLeft(s).isLeft }

  "empty toLeft is right" ! forAll { s: String => empty.toLeft(s).isRight }

  "just toRight is right" ! forAll { (x: Int, s: String) => just(x).toRight(s).isRight }

  "empty toRight is left" ! forAll { s: String => empty.toRight(s).isLeft }

  "just isJust" ! forAll { x: Int => just(x).isJust }

  "just isn't empty" ! forAll { x: Int => !just(x).isEmpty }

  "empty is empty" ! check(empty.isEmpty)

  "empty isn't just" ! check(!empty.isJust)

  "empty pattern matches" ! check {
    empty match {
      case Empty() => true
      case _       => false
    }
  }

  "just to option is some" ! forAll { x: Int => just(x).toOption.isDefined }

  "empty to option is none" ! check(empty.toOption.isEmpty)

  "just orElse is just" ! forAll { (x: Int, m: Maybe[Int]) => just(x).orElse(m).isJust }

  "fromNullable(null) is Empty" ! check {
    val s: String = null
    Maybe.fromNullable(s).isEmpty
  }

  "fromNullable(notNull) is just" ! forAll { (s: String) => Maybe.fromNullable(s) must_=== just(s) }

  final class Unfoo(val s: String, val i: Int)
  object Unfoo {
    def apply(s: String, i: Int): Unfoo = new Unfoo(s, i)
    def unapply(u: Unfoo): Maybe.Just[(String, Int)] = Maybe.Just((u.s, u.i))
  }
  "Just can be used in custom unapply" ! {
    val Unfoo(_, _) = Unfoo("hello", 1)
  }

  "attempt" in {
    Maybe.attempt("foo".toInt) must_=== Maybe.empty

    Maybe.attempt("1".toInt) must_=== Maybe.just(1)
  }

  "Maybe addition should terminate when encountering the first Just" in {
    val P = PlusEmpty[Maybe]

    val f: Int => Maybe[(Maybe[String], Int)] = i => {
      if (i > 0) just((Maybe.empty, i-1))
      else if (i == 0) just((just("Stop"), i-1))
      else sys.error("BOOM!")
    }

    P.unfoldrPsum(5)(f) must_=== Just("Stop")
  }

  "lifted Monoid is short-circuiting" in {
    val M: Monoid[Maybe[Int]] = Monoid.liftMonoid

    val f: Int => Maybe[(Maybe[Int], Int)] = i => {
      if (i > 0) just((just(i), i-1))
      else if (i == 0) just((empty, i-1))
      else sys.error("BOOM!")
    }

    M.unfoldrSum(5)(f) must_=== Empty()
  }

  "lifted PlusEmpty is short-circuiting" in {
    import scalaz.std.list._

    val P: PlusEmpty[λ[a => Maybe[List[a]]]] = PlusEmpty.liftPlusEmpty[Maybe, List]

    val f: Int => Maybe[(Maybe[List[Int]], Int)] = i => {
      if (i > 0) just((just(List.range(0, i)), i-1))
      else if (i == 0) just((empty, i-1))
      else sys.error("BOOM!")
    }

    P.unfoldrPsum(5)(f) must_=== Empty()
  }

  "lifted Reducer is short-circuiting" in {
    val R: Reducer[Maybe[Int], Maybe[Int]] = Apply[Maybe].liftReducer(Reducer.identityReducer[Int])

    val f: Int => Maybe[(Maybe[Int], Int)] = i => {
      if (i > 0) just((just(i), i-1))
      else if (i == 0) just((empty, i-1))
      else sys.error("BOOM!")
    }

    R.unfoldrOpt(5)(f) must_=== Just(Empty())
  }

  object instances {
    def equal[A: Equal] = Equal[Maybe[A]]
    def order[A: Order] = Order[Maybe[A]]
    def semigroup[A: Semigroup] = Monoid[Maybe[A]]
    def semiLattice[A: SemiLattice] = SemiLattice[Maybe[A]]
    def bindRec = BindRec[Maybe]
    def monad = Monad[Maybe]

    def monoidFirst[A] = Monoid[Maybe[A] @@ First]
    def monoidLast[A] = Monoid[Maybe[A] @@ Last]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[Maybe[A]]
    def monoid[A: SemiLattice] = Monoid[Maybe[A]]
  }
}
