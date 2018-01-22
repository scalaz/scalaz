package scalaz

object MemoTest extends SpecLite {

  "Memo.doubleArrayMemo should not throw exceptions" in {
    import scalaz.std.anyVal._

    var doubler_count = 0
    def doubler(n: Int): Double = {
      doubler_count += 1
      n * 2.0
    }

    val memo = Memo.doubleArrayMemo(10)
    val mdoubler = memo(doubler)

    mdoubler(1) must_=== 2.0
    doubler_count must_=== 1
    mdoubler(1) must_=== 2.0
    doubler_count must_=== 1

    // outside the memoisation zone
    mdoubler(20) must_=== 40.0
    doubler_count must_=== 2
    mdoubler(20) must_=== 40.0
    doubler_count must_=== 3

    mdoubler(-1) must_=== -2.0
    doubler_count must_=== 4
    mdoubler(-1) must_=== -2.0
    doubler_count must_=== 5
  }

  "Memo.doubleArrayMemo should support Double.NaN" in {
    import scalaz.std.anyVal._

    var doubler_count = 0
    def doubler(n: Int): Double = {
      doubler_count += 1
      n * 2.0
    }

    val memo = Memo.doubleArrayMemo(10, Double.NaN)
    val mdoubler = memo(doubler)

    mdoubler(0) must_=== 0.0
    doubler_count must_=== 1
    mdoubler(0) must_=== 0.0
    doubler_count must_=== 1
  }

}
