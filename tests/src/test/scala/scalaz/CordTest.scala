package scalaz

import org.scalacheck.{ Arbitrary, Gen }
import scalaz.scalacheck.ScalazProperties._

import scalaz.syntax.enum._
import scalaz.std.anyVal._
import scalaz.std.string._

object CordTest extends SpecLite {
  ".toString must produce expected output" in {
    val lefts = (0 |-> 9).foldLeft(Cord())((acc, i) => acc ++ Cord(i.toString))
    lefts.toString.must_===(lefts.shows)
    lefts.toString.must_===("0123456789")

    val rights = (0 |-> 9).foldRight(Cord())((i, acc) => Cord(i.toString) ++ acc)
    rights.toString.must_===(rights.shows)
    rights.toString.must_===(lefts.toString)
  }

  ".shows must be stack safe" in {
    val nums = (1 |-> 99999).map(_.toString)
    val lefts = nums.foldLeft(Cord())((acc, i) => acc ++ Cord(i))
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

    val rights = nums.foldRight(Cord())((i, acc) => Cord(i) ++ acc)

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

  // avoid stack overflows...
  def fallback(c: Cord, backup: Cord.Leaf): Cord = c match {
    case Cord.Branch(d, _, _) if d >= 5 => backup
    case _ => c
  }
  lazy val genLeaf = for {
    str <- Arbitrary.arbitrary[String]
  } yield Cord.Leaf(str)
  lazy val genBranch = for {
    backup1 <- genLeaf
    backup2 <- genLeaf
    left <- genCord
    right <- genCord
  } yield Cord.Branch(fallback(left, backup1), fallback(right, backup2))
  lazy val genCord: Gen[Cord] = Gen.oneOf(genLeaf, genBranch)

  implicit def ArbitraryCord: Arbitrary[Cord] = Arbitrary(genCord)

  checkAll(monoid.laws[Cord])
  checkAll(equal.laws[Cord])
}
