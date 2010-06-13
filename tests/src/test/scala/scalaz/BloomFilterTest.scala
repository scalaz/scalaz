package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck._
import Gen._
import Prop._
import Arbitrary._
import scalacheck.ScalaCheckBinding
import ScalaCheckBinding._
import Scalaz._

class BloomFilterTest extends Specification with Sugar with ScalaCheck {
  def filter(added: Set[Int]): BloomFilter = {
    val filter = new BloomFilter(added.size * 10, added.size)
    for (a <- added) filter add a.hashCode
    filter
  }

  def genLargeUniformlyDistributedSet: Gen[Set[Int]] =
    listOfN(5000, resize(java.lang.Integer.MAX_VALUE, arbitrary[Int])) ∘ ((_: List[Int]).toSet)

  "filter false negatives" in {    
    genLargeUniformlyDistributedSet must pass {(is: Set[Int]) =>
      is mustNot beEmpty
      val f = filter(is)
      is.forall(i => f contains i.hashCode)
    }
  }

  "filter false positive rate" in {
    skip("broken")
    val testData = genLargeUniformlyDistributedSet <|*|> genLargeUniformlyDistributedSet      
    testData must pass ({
      (inFilter: Set[Int], additional: Set[Int]) => {
        val f = filter(inFilter)
        val falsePositives = for{
          num <- additional
          if !additional.contains(num)
          if f.contains(num.hashCode)
        } yield num

        val actualFalsePositiveRate = 1.0 * falsePositives.length / additional.size
        val ratio = f.expectedFalsePositiveProbability / actualFalsePositiveRate
        ratio > 0.5 && ratio < 2.0
      }
    }.tupled)
  }
}
