package scalaz

import org.scalacheck.Gen
import org.specs.{Specification, ScalaCheck}

/**
 * CountingBloomFilterTest
 *
 * @author everpeace _at_ gmail _dot_ com
 * @date 11/05/04
 */
class CountingBloomFilterTest extends Specification with ScalaCheck {

  import CountingBloomFilter._
  import org.scalacheck.Prop.forAll
  import org.scalacheck.Gen._

  def countsNotChanged(filter: CountingBloomFilter) = forAll(sample)((s: String) => {
    filter.add(s)
    filter.discard(s)
    filter.counts == 0
  })

  "Counts does not change after add and discard." in {
    val filter = CountingBloomFilter(0.9, 100)
    countsNotChanged(filter) must pass
  }

  def NoFalsePositive(filter: CountingBloomFilter) =
    forAll(sample)(s => {
      filter.add(s)
      filter.contains(s)
    })

  "No false negative results." in {
    val filter = CountingBloomFilter(0.99999999999, 100)
    NoFalsePositive(filter) must pass
  }

  "False positive prob. is less than max false positive prob. set." in {
    val p: Double = 0.0001
    val n: Int = 100000
    val filter = CountingBloomFilter(p, n)
    System.out.println("[FilterInfo] filter-bit-size:%d, number of hash:%d, max false positive prob.: %1.7f".format(filter.size, filter.k, p))

    // add 2n distinct strings
    val added: List[String] = genStrings(2 * n)
    for (str <- added) filter.add(str)

    // discard n distinct strings
    val discarded = added.toList.slice(0, n)
    for (str <- discarded) filter.discard(str)

    // measure false positive prob. for discarded strings
    val fpCount = (1 /: (discarded.map(filter.contains(_)))) {
      (c: Int, b: Boolean) => if (b) c + 1 else c
    }

    System.out.println("[mesured false positive prob.] %1.7f (%d false positives found in %d trials)".format(fpCount.toDouble / n, fpCount, n))
    (fpCount.toDouble / n) must lessThan(p)
  }

  // generator for string with length l
  private def str(i: Int) = for (cs <- listOfN(i, Gen.alphaNumChar)) yield cs.mkString

  // generator for AlphaNum^[1..6]
  private def sample: Gen[String] = Gen.frequency((1, str(1)), (2, str(2)), (3, str(3)), (4, str(4)), (5, str(4)), (6, str(4)))

  // generate num distinct random strings
  private def genStrings(num: Int): List[String] = {
    var added: Set[String] = Set.empty
    var numAdded = 0
    while (numAdded < num) {
      sample.sample match {
        case Some(s) => {
          if (!added.contains(s)) {
            added = added + s
            numAdded += 1
          }
        }
        case _ => {}
      }
    }
    added.toList
  }
}