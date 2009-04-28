package scalaz.test

sealed trait Rand {
  def choose(seed: Option[Long], from: Int, to: Int): Int
  def choose(seed: Option[Long], from: Double, to: Double): Double

  def chooseInt(seed: Long, from: Int, to: Int): Int = choose(Some(seed), from, to)
  def chooseInt(from: Int, to: Int): Int = choose(None, from, to)

  def chooseDouble(seed: Long, from: Double, to: Double): Double = choose(Some(seed), from, to)
  def chooseDouble(from: Double, to: Double): Double = choose(None, from, to)

  def reseed(seed: Long) = Rand.rand((_, from, to) => chooseInt(seed, from, to), (_, from, to) => chooseDouble(seed, from, to))
}

object Rand {
  def rand(f: (Option[Long], Int, Int) => Int, g: (Option[Long], Double, Double) => Double) = new Rand {
    def choose(seed: Option[Long], from: Int, to: Int) = f(seed, from, to)
    def choose(seed: Option[Long], from: Double, to: Double) = g(seed, from, to)
  }

  import java.util.Random

  implicit val StandardRand: Rand = rand((seed, from, to) => {
    val k = seed match {
      case Some(v) => new Random(v)
      case None => new Random
    }
    val f = Math.min(from, to);
    val t = Math.max(from, to);
    k.nextInt(t - f + 1)
  }, (seed, from, to) => {
    val k = seed match {
      case Some(v) => new Random(v)
      case None => new Random
    }
    val f = Math.min(from, to);
    val t = Math.max(from, to);
    k.nextDouble * (t - f) + f
  })
}