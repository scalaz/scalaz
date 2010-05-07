package scalaz

import java.math.BigInteger

sealed trait BigIntegerMultiplication extends NewType[BigInteger]

sealed trait BigIntMultiplication extends NewType[BigInt]

sealed trait ByteMultiplication extends NewType[Byte]

sealed trait CharMultiplication extends NewType[Char]

sealed trait IntMultiplication extends NewType[Int]

sealed trait LongMultiplication extends NewType[Long]

sealed trait ShortMultiplication extends NewType[Short]

trait Multiplications {
  def multiplication(n: BigInteger): BigIntegerMultiplication = new BigIntegerMultiplication {
    val value = n
  }

  def multiplication(n: BigInt): BigIntMultiplication = new BigIntMultiplication {
    val value = n
  }

  def multiplication(n: Byte): ByteMultiplication = new ByteMultiplication {
    val value = n
  }

  def multiplication(n: Char): CharMultiplication = new CharMultiplication {
    val value = n
  }
  
  def multiplication(n: Int): IntMultiplication = new IntMultiplication {
    val value = n
  }

  def multiplication(n: Long): LongMultiplication = new LongMultiplication {
    val value = n
  }

  def multiplication(n: Short): ShortMultiplication = new ShortMultiplication {
    val value = n
  }
}



