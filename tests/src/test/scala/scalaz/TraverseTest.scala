package scalaz

import java.math.BigInteger
import scalacheck.{ScalazProperties, ScalazArbitrary, ScalaCheckBinding}
import org.scalacheck.{Gen, Arbitrary}

class TraverseTest extends Spec {

  import scalaz._
  import std.string._, std.tuple._, std.anyVal._, std.option._
  import syntax.traverse._, syntax.equal._
  import Ident._
  import WriterT._

  "list" should {
    import std.list._

    // ghci> import Data.Traversable
    // ghci> import Control.Monad.Writer
    // ghci> let (|>) = flip ($)
    // ghci> traverse (\x -> writer (x, x)) ["1", "2", "3"] |> runWriter
    // (["1","2","3"],"123")
    "apply effects in order" in {
      val s: Writer[String, List[Int]] = List(1, 2, 3).traverseU(x => Writer(x.toString, x))
      s.runT must be_=== (("123", List(1, 2, 3)))
    }

    "traverse through option effect" in {
      val s: Option[List[Int]] = List(1, 2, 3).traverseU((x: Int) => if (x < 3) some(x) else none)
      s must be_===(none[List[Int]])
    }

    "not blow the stack" in {
      val s: Option[List[Int]] = List.range(0, 32 * 1024).traverseU(x => some(x))
      s.map(_.take(3)) must be_===(some(List(0, 1, 2)))
    }
  }

  "stream" should {
    import std.stream._

    "apply effects in order" in {
      val s: Writer[String, Stream[Int]] = Stream(1, 2, 3).traverseU(x => Writer(x.toString, x))
      s.runT must be_===(("123", Stream(1, 2, 3)))
    }

    // ghci> import Data.Traversable
    // ghci> traverse (\x -> if x < 3 then Just x else Nothing) [1 ..]
    // Nothing
    "allow partial traversal" in {
      skipped("TODO") // TODO
      val stream = Stream.from(1)
      val s: Option[Stream[Int]] = stream.traverseU((x: Int) => if (x < 3) some(x) else none)
      s must be_===(none)
    }
  }
}