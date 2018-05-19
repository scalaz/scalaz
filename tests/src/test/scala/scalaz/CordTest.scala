package scalaz

import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop.forAll

import scalaz.scalacheck.ScalazProperties._
import scalaz.syntax.enum._
import scalaz.syntax.monoid._
import scalaz.std.anyVal._
import scalaz.std.string._

object CordTest extends SpecLite {

  ".toString should be .shows" ! forAll { c: Cord =>
    // potentially flushes out a bunch of problems, but the cords aren't that
    // big so don't expect it to catch any stack overflow bugs.
    c.shows.must_===(c.toString)
  }

  ":: cons must produce the correct output" in {
    (Cord("hello") :: Cord(" ") :: Cord("world")).shows.must_===("hello world")
  }

  "++ strict append must produce the correct output" in {
    (Cord("hello") ++ Cord(" ") ++ Cord("world")).shows.must_===("hello world")
  }

  ".shows must produce expected output" in {
    val nums = (0 |-> 9).map(_.toString)

    val lefts = nums.foldLeft(Cord())((acc, i) => acc |+| Cord(i))
    lefts.shows.must_===(lefts.shows)
    lefts.shows.must_===("0123456789")

    val rights = nums.foldRight(Cord())((i, acc) => Cord(i) |+| acc)
    rights.shows.must_===(rights.shows)
    rights.shows.must_===(lefts.shows)
  }

  ".shows must be stack safe" in {
    val nums = (1 |-> 99999).map(_.toString)
    val lefts = nums.foldLeft(Cord())((acc, i) => acc |+| Cord(i))
    lefts.shows.length.must_===(488889)

    // it's ok to change these assertions if you optimise the representation, so
    // long as they are asserting the expectation that foldLeft / foldRight
    // produce expected structures, and that you've stressed tested stack safety
    // with any new corner cases that your repr may be susceptible to. This
    // level of detail is not exposed to the public API.

    lefts match {
      case Cord.Branch(8, _, Cord.Leaf("99999")) =>
      case Cord.Branch(x, _, _) => fail(s"lefts: unexpected depth $x")
      case _ => fail("lefts: unexpected leaf")
    }

    val rights = nums.foldRight(Cord())((i, acc) => Cord(i) |+| acc)

    // prefer minimal error output when these fail, otherwise they spam the buffer
    assert(rights.shows == lefts.shows)
    assert(lefts === rights)

    rights match {
      case Cord.Branch(1, Cord.Leaf("1"), _) =>
      case Cord.Branch(x, _, _) => fail(s"rights: unexpected depth $x")
      case _ => fail("rights: unexpected leaf")
    }
  }

  "cord interpolator should produce expected Cords" in {
    import scalaz.syntax.show._

    // trivial strings
    cord"hello".shows.must_===("hello")

    // interpolate Cord values
    {
      val a = Cord("hello")
      val b = Cord("world")
      cord"$a $b".shows.must_===("hello world")
    }

    // interpolate via Show instances
    {
      val a = "hello"
      val b = "world"
      cord"$a $b".shows.must_===("\"hello\" \"world\"")
      cord" $a $b ".shows.must_===(" \"hello\" \"world\" ")
    }

    // handle escape characters
    {
      val a = "hello"
      val b = "world"
      cord"$a\n$b".shows.must_===("\"hello\"\n\"world\"")

      cord"""
$a

$b
""".shows.must_===("\n\"hello\"\n\n\"world\"\n")
    }
  }

  // the Arbitrary is very limited in the shapes it can create, to avoid stack
  // overflows when generating the recursive data... *sigh*
  lazy val genLeaf: Gen[Cord.Leaf] = for {
    str <- Gen.alphaNumStr
  } yield Cord.Leaf(str)
  lazy val genLeft: Gen[Cord] = for {
    left <- genLeaf
    right <- genLeaf
  } yield Cord.Branch(left, right)
  lazy val genRight: Gen[Cord] = Gen.listOf(genLeft).map { cs =>
    cs.foldRight(Cord())((i, acc) => i |+| acc)
  }
  lazy val genBranch: Gen[Cord] = for {
    left <- Gen.oneOf(genLeft, genLeaf)
    right <- Gen.oneOf(genRight, genLeaf)
  } yield Cord.Branch(left, right)
  lazy val genCord: Gen[Cord] = Gen.oneOf(genLeaf, genBranch)

  implicit def ArbitraryCord: Arbitrary[Cord] = Arbitrary(genCord)

  checkAll(monoid.laws[Cord])
  checkAll(equal.laws[Cord])
}
