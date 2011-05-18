package scalaz

class BloomFilter(val size: Int, val expectedElements: Int) {
  require(size > 0)
  require(expectedElements > 0)

  import scala.math._

  private[this] val bitArray = new BitArray(size)
  private[this] val k = ceil((bitArray.size / expectedElements) * log(2.0)).toInt
  lazy val expectedFalsePositiveProbability = pow(1 - exp(-k * 1.0 * expectedElements / bitArray.size), k)

  def add(hash: Int) {
    def add(i: Int, seed: Int) {
      if (i == k) return
      val next = xorRandom(seed)
      bitArray.set(next)
      add(i + 1, next)
    }
    add(0, hash)
  }

  def contains(hash: Int): Boolean = {
    def contains(i: Int, seed: Int): Boolean = {
      if (i == k) return true
      val next = xorRandom(seed)
      if (!bitArray.get(next)) return false
      return contains(i + 1, next)
    }
    contains(0, hash)
  }

  private def xorRandom(i: Int) = {
    var y = i
    y ^= y << 13
    y ^= y >> 17
    y ^ y << 5
  }

  private class BitArray(bits: Int) {
    val size = (1 << 6) max nextPow2(bits)
    require(isPowerOf2(size))
    private val data = new Array[Long](size >> 6)

    def set(index: Int) = data(idx(index)) |= (1L << index)

    def get(index: Int) = (data(idx(index)) & (1L << index)) != 0

    private val mask = size - 1

    private def idx(index: Int) = (index & mask) >> 6

    private def isPowerOf2(i: Int) = ((i - 1) & i) == 0

    private def nextPow2(i: Int) = {
      def highestBit(remainder: Int, c: Int): Int = if (remainder > 0) highestBit(remainder >> 1, c + 1) else c
      require(i <= (1 << 30), "size was %s has to be smaller or equal to %s" format (i, 1 << 30))
      val n = if (isPowerOf2(i)) i else 1 << highestBit(i, 0)
      assert(n >= i && i * 2 > n && isPowerOf2(n))
      n
    }
  }
}

