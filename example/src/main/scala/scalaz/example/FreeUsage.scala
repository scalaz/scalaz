package scalaz.example

import scalaz.{ Free, Monad, ~> }
import scalaz.std.function._
import scalaz.syntax.monad._
import scalaz.effect.IO

import scala.util.Random

// Example usage of free monad
object FreeUsage extends App {

  // An algebra of primitive operations in the context of a random number generator
  sealed trait RngOp[A]
  object RngOp {
    case object NextBoolean              extends RngOp[Boolean]
    case object NextDouble               extends RngOp[Double]
    case object NextFloat                extends RngOp[Float]
    case object NextGaussian             extends RngOp[Double]
    case object NextInt                  extends RngOp[Int]
    case class  NextIntInRange(max: Int) extends RngOp[Int]
    case object NextLong                 extends RngOp[Long]
    case object NextPrintableChar        extends RngOp[Char]
    case class  NextString(length: Int)  extends RngOp[String]
    case class  SetSeed(seed: Long)      extends RngOp[Unit]
  }

  // Free monad over the free functor of RngOp.
  type Rng[A] = Free[RngOp, A]

  // Smart constructors for Rng[A]
  val nextBoolean              = Free.liftF(RngOp.NextBoolean)
  val nextDouble               = Free.liftF(RngOp.NextDouble)
  val nextFloat                = Free.liftF(RngOp.NextFloat)
  val nextGaussian             = Free.liftF(RngOp.NextGaussian)
  val nextInt                  = Free.liftF(RngOp.NextInt)
  def nextIntInRange(max: Int) = Free.liftF(RngOp.NextIntInRange(max))
  val nextLong                 = Free.liftF(RngOp.NextLong)
  val nextPrintableChar        = Free.liftF(RngOp.NextPrintableChar)
  def nextString(length: Int)  = Free.liftF(RngOp.NextString(length))
  def setSeed(seed: Long)      = Free.liftF(RngOp.SetSeed(seed))

  // You can of course derive new operations from the primitives
  def nextNonNegativeInt       = nextInt.map(_.abs)
  def choose[A](h: A, tl: A*)  = nextIntInRange(tl.length + 1).map((h +: tl).apply)

  // Natural transformation to (Random => A)
  type RandomReader[A] = Random => A
  val toState: RngOp ~> RandomReader =
    new (RngOp ~> RandomReader) {
      def apply[A](fa: RngOp[A]) =
        fa match {
          case RngOp.NextBoolean       => _.nextBoolean
          case RngOp.NextDouble        => _.nextDouble
          case RngOp.NextFloat         => _.nextFloat
          case RngOp.NextGaussian      => _.nextGaussian
          case RngOp.NextInt           => _.nextInt
          case RngOp.NextIntInRange(n) => _.nextInt(n)
          case RngOp.NextLong          => _.nextLong
          case RngOp.NextPrintableChar => _.nextPrintableChar
          case RngOp.NextString(n)     => _.nextString(n)
          case RngOp.SetSeed(n)        => _.setSeed(n)
      }
    }

  // Now we have enough structure to run a program
  def runRng[A](program: Rng[A], seed: Long): A =
    program.foldMap(toState).apply(new Random(seed))

  // Syntax
  implicit class RngOps[A](ma: Rng[A]) {
    def exec(seed: Long): A = runRng(ma, seed)
    def liftIO: IO[A] = IO(System.currentTimeMillis).map(exec)
  }

  // An example that returns a pair of integers, a < 100, b < a and a color
  val prog: Rng[(Int, Int, String)] =
    for {
      a <- nextIntInRange(100)
      b <- nextIntInRange(a)
      c <- choose("red", "green", "blue")
    } yield (a, b, c)

  // Run that baby
  println(prog.exec(0L))   // pure! always returns (60,28,green)
  println(prog.exec(0L))   // exactly the same of course
  println(prog.exec(123L)) // (82,52,blue)
  println(prog.liftIO.unsafePerformIO) // DANGER: impure, who knows what will happen?

  // Of course all the normal combinators work
  println(nextBoolean.replicateM(10).exec(0L))

}

